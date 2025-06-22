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
-type config() :: mstmnte_db:config().

%% ------------------------------------------------------------------
%% Public API — thin wrappers that allow an optional 3rd Opts argument.
%% ------------------------------------------------------------------

list(Req, State)              -> list(Req, State, #{}).
get(Req, State)               -> get(Req, State, #{}).
bulk_get(Req, State)          -> bulk_get(Req, State, #{}).
maint_list(Req, State)        -> maint_list(Req, State, #{}).
maint_get(Req, State)         -> maint_get(Req, State, #{}).
maint_patch(Req, State)       -> maint_patch(Req, State, #{}).

%% ------------------------------------------------------------------
%%  Implementation
%% ------------------------------------------------------------------

list(Req0, State, Opts) ->
    Config = db_config(Opts),
    case mstmnte_db:list(Config) of
        {ok, Docs} ->
            send_json(Req0, State, 200, Docs);
        {error, no_db} ->
            send_json(Req0, State, 500, #{error => no_db})
    end.


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


maint_list(Req0, State, Opts) ->
    %% Currently identical to list/3; kept separate for future rules.
    list(Req0, State, Opts).


maint_get(Req0, State, Opts) ->
    get(Req0, State, Opts).


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

db_config(Opts) ->
    maps:get(klsn_db, Opts, #{}).


send_json(Req0, State, Status, Data) ->
    Json = jsone:encode(Data),
    Req1 = cowboy_req:reply(Status, #{ <<"content-type">> => <<"application/json">> }, Json, Req0),
    {ok, Req1, State}.


%% Reads the request body and decodes it as JSON.
read_json_body(Req0) ->
    {ok, BodyBin, Req1} = cowboy_req:read_body(Req0),
    {ok, jsone:decode(BodyBin), Req1}.


%% Bulk get helper — when Ids == all we return a map of all masters.
bulk_get_docs(all, Config) ->
    case mstmnte_db:list(Config) of
        {ok, Docs} ->
            {ok, lists_to_map(Docs)};
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


lists_to_map(List) -> maps:from_list([{maps:get(<<"_id">>, D), D} || D <- List]).
