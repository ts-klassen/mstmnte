-module(mstmnte_handler).

%% ------------------------------------------------------------------
%% HTTP helpers that turn a Cowboy req/state pair into JSON replies by
%% delegating the persistence logic to mstmnte_db.
%%
%% The module does **not** depend on Cowboy at compile-time; it merely
%% performs *remote* calls (cowboy_req:reply/4 etc.). Therefore a build
%% that does not include Cowboy still succeeds — the calls will only be
%% evaluated at runtime inside an actual Cowboy environment.
%% ------------------------------------------------------------------

-export([
    list/2,
    list/3,
    get/2,
    get/3,
    bulk_get/2,
    bulk_get/3,
    maint_list/2,
    maint_list/3,
    maint_get/2,
    maint_get/3,
    maint_patch/2,
    maint_patch/3
]).

-include_lib("kernel/include/logger.hrl").

%% ------------------------------------------------------------------
%% Types (kept minimal to avoid Cowboy dependency).
%% ------------------------------------------------------------------

-type req()    :: term().
-type state()  :: term().

-export_type([req/0, state/0]).

%% ------------------------------------------------------------------
%% Public API — thin wrappers that allow an optional 3rd Opts argument.
%% ------------------------------------------------------------------

-spec list(req(), state()) -> {ok, req(), state()}.
list(Req, State)              -> list(Req, State, #{}).
-spec get(req(), state()) -> {ok, req(), state()}.
get(Req, State)               -> get(Req, State, #{}).
-spec bulk_get(req(), state()) -> {ok, req(), state()}.
bulk_get(Req, State)          -> bulk_get(Req, State, #{}).
-spec maint_list(req(), state()) -> {ok, req(), state()}.
maint_list(Req, State)        -> maint_list(Req, State, #{}).
-spec maint_get(req(), state()) -> {ok, req(), state()}.
maint_get(Req, State)         -> maint_get(Req, State, #{}).
-spec maint_patch(req(), state()) -> {ok, req(), state()}.
maint_patch(Req, State)       -> maint_patch(Req, State, #{}).

%% ------------------------------------------------------------------
%%  Implementation
%% ------------------------------------------------------------------

-spec list(req(), state(), mstmnte:opts()) -> {ok, req(), state()}.
list(Req0, State, Opts) ->
    Config = db_config(Opts),
    case mstmnte_db:list(Config) of
        {ok, Ids} ->
            send_json(Req0, State, 200, Ids);
        {error, no_db} ->
            send_json(Req0, State, 500, #{error => no_db})
    end.


-spec get(req(), state(), mstmnte:opts()) -> {ok, req(), state()}.
get(Req0, State, Opts) ->
    Config = db_config(Opts),
    {Id, Req1} = cowboy_req:binding(<<"id">>, Req0),
    case mstmnte_db:get(Id, Config) of
        {ok, Doc} ->
            send_json(Req1, State, 200, Doc);
        {error, not_found} ->
            send_json(Req1, State, 404, #{error => not_found});
        {error, no_db} ->
            send_json(Req1, State, 500, #{error => no_db})
    end.


-spec bulk_get(req(), state(), mstmnte:opts()) -> {ok, req(), state()}.
bulk_get(Req0, State, Opts) ->
    Config = db_config(Opts),
    {ok, Body, Req1} = read_json_body(Req0),
    Ids = case Body of
              [] -> all;
              L when is_list(L) -> L;
              _ -> error(bad_request)
          end,
    case bulk_get_docs(Ids, Config) of
        {ok, DocsMap} -> send_json(Req1, State, 200, DocsMap);
        {error, Error} when Error =:= not_found orelse Error =:= no_db ->
            Status = case Error of not_found->404; _->500 end,
            send_json(Req1, State, Status, #{error => Error})
    end.


-spec maint_list(req(), state(), mstmnte:opts()) -> {ok, req(), state()}.
maint_list(Req0, State, Opts) ->
    %% Currently identical to list/3; kept separate for future rules.
    list(Req0, State, Opts).


-spec maint_get(req(), state(), mstmnte:opts()) -> {ok, req(), state()}.
maint_get(Req0, State, Opts) ->
    get(Req0, State, Opts).


-spec maint_patch(req(), state(), mstmnte:opts()) -> {ok, req(), state()}.
maint_patch(Req0, State, Opts) ->
    Config = db_config(Opts),
    {ok, Doc, Req1} = read_json_body(Req0),
    case maps:is_key(<<"_id">>, Doc) of
        true -> ok;
        false -> error(bad_request)
    end,
    case mstmnte_db:upsert(Doc, Config) of
        {ok, Saved} -> send_json(Req1, State, 200, Saved);
        {error, conflict} -> send_json(Req1, State, 409, #{error => conflict});
        {error, no_db} -> send_json(Req1, State, 500, #{error => no_db})
    end.


%% ------------------------------------------------------------------
%% Helper fns
%% ------------------------------------------------------------------
%% Return the DB configuration map extracted from Opts.
-spec db_config(mstmnte:opts()) -> map().
db_config(Opts) ->
    maps:get(klsn_db, Opts, #{}).

%% Encode Data as JSON and send it with the given HTTP status.
-spec send_json(req(), state(), integer(), term()) -> {ok, req(), state()}.
send_json(Req0, State, Status, Data) ->
    Json = jsone:encode(Data),
    Req1 = cowboy_req:reply(Status, #{ <<"content-type">> => <<"application/json">> }, Json, Req0),
    {ok, Req1, State}.


%% Reads the request body and decodes it as JSON.
-spec read_json_body(req()) -> {ok, term(), req()}.
read_json_body(Req0) ->
    {ok, BodyBin, Req1} = cowboy_req:read_body(Req0),
    {ok, jsone:decode(BodyBin), Req1}.


%% Bulk get helper — when Ids == all we return a map of all masters.
-spec bulk_get_docs(all | list(), map()) -> {ok, map()} | {error, term()}.
bulk_get_docs(all, Config) ->
    case mstmnte_db:list(Config) of
        {ok, Ids} -> bulk_get_docs(Ids, Config);
        Error -> Error
    end;
bulk_get_docs(Ids, Config) when is_list(Ids) ->
    %% Fetch individually and accumulate into a map.
    {Docs, Missing} = lists:foldl(fun(Id, {Acc, Miss}) ->
                                          case mstmnte_db:get(Id, Config) of
                                              {ok, Doc} -> {[{Id, Doc}|Acc], Miss};
                                              {error, not_found} -> {Acc, [Id|Miss]};
                                              Err = {error, _} -> throw(Err)
                                          end
                                  end, {[], []}, Ids),
    case Missing of
        [] -> {ok, maps:from_list(Docs)};
        _  -> {error, not_found}
    end.


%% Not used anymore.

