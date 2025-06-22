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

-type id()     :: klsn:binstr().
-type doc()    :: map().
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
-spec list(config()) -> {ok, [doc()]} | {error, no_db}.
list(_Config) ->
    todo.


%% @doc
%% Fetch the document identified by *Id*.
%%
-spec get(id(), config()) -> {ok, doc()} | {error, no_db | not_found}.
get(_Id, _Config) ->
    todo.


%% @doc
%% Create or replace *Doc* in the configured database.
%%
-spec upsert(doc(), config()) -> {ok, doc()} | {error, no_db | conflict}.
upsert(_Doc, _Config) ->
    todo.
