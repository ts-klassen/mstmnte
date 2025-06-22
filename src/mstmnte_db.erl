-module(mstmnte_db).

%% ------------------------------------------------------------------
%% Very small database façade meant to isolate the rest of the `mstmnte`
%% code base from the actual CouchDB (klsn_db) implementation.
%% ------------------------------------------------------------------

-export([
    list/1,
    get/2,
    upsert/2
]).

%% ------------------------------------------------------------------
%% TYPES
%% ------------------------------------------------------------------

-type id()     :: klsn_db:id().
-type doc()    :: klsn_db:payload().
-type config() :: #{ db_name := klsn_db:db()
                   , db_info := klsn_db:info()
                   }.

%% ------------------------------------------------------------------
%% SPECS & STUBS
%% ------------------------------------------------------------------

%% @doc
%% Return a list of every document stored in the database configured by
%% *Config*.
%%
-spec list(config()) -> {ok, [id()]} | {error, no_db}.
list(#{db_name:=DB, db_info:=Info}) ->
    case klsn_db:lookup(DB, {raw, <<"/_all_docs">>}, Info) of
        {value, Doc} ->
            Ids = [Id || #{<<"id">> := Id} <- maps:get(<<"rows">>, Doc, [])],
            {ok, Ids};
        none ->
            {error, no_db}
    end.


%% @doc
%% Fetch the document identified by *Id*.
%%
-spec get(id(), config()) -> {ok, doc()} | {error, no_db | not_found}.
get(Id, #{db_name:=DB, db_info:=Info}) ->
    case klsn_db:lookup(DB, Id, Info) of
        {value, Doc} ->
            {ok, Doc};
        none ->
            case klsn_db:lookup(DB, {raw, <<"/">>}, Info) of
                {value, _} ->
                    {error, not_found};
                none ->
                    {error, no_db}
            end
    end.


%% @doc
%% Create or replace *Doc* in the configured database.
%%
-spec upsert(doc(), config()) -> {ok, doc()} | {error, no_db | conflict}.
upsert(Doc=#{<<"_id">>:=Id}, #{db_name:=DB, db_info:=Info}) ->
    try
        klsn_db:upsert(DB, Id, fun
            (none) ->
                Doc;
            ({value, Original}) ->
                case {Original, Doc} of
                    {#{<<"_rev">>:=Rev}, #{<<"_rev">>:=Rev}} ->
                        ok;
                    {_, #{<<"_rev">>:=_Rev}} ->
                        throw({?MODULE, conflict});
                    _ ->
                        ok
                end,
                maps:merge(Original, Doc)
        end, Info)
    of
        Updated ->
            {ok, Updated}
    catch
        throw:{?MODULE, conflict} ->
            {error, conflict};
        error:not_found ->
            {error, no_db}
    end.
