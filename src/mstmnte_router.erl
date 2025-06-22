-module(mstmnte_router).

-export([api/0, webui/0]).

%% ------------------------------------------------------------------
%% Public helpers that return route lists ready for cowboy_router:compile/1
%% ------------------------------------------------------------------

api() ->
    Opts = #{klsn_db => db_opts()},
    [
        %% Master endpoints
        { 'GET',  <<"/mstmnte/master">>,          mstmnte_route, {mstmnte_handler, list, Opts}},
        { 'POST', <<"/mstmnte/master">>,          mstmnte_route, {mstmnte_handler, bulk_get, Opts}},
        { 'GET',  <<"/mstmnte/master/:id">>,      mstmnte_route, {mstmnte_handler, get, Opts}},

        %% Maintenance endpoints
        { 'GET',  <<"/mstmnte/maintenance">>,     mstmnte_route, {mstmnte_handler, maint_list, Opts}},
        { 'GET',  <<"/mstmnte/maintenance/:id">>, mstmnte_route, {mstmnte_handler, maint_get, Opts}},
        { 'PATCH',<<"/mstmnte/maintenance">>,     mstmnte_route, {mstmnte_handler, maint_patch, Opts}}
    ].

db_opts() ->
    User = application:get_env(mstmnte, klsn_db, #{}),
    maps:merge(#{db_name => <<"mstmnte">>, db_info => #{}}, User).

webui() ->
    [
        {'GET', <<"/mstmnte/webui/[...]">>, cowboy_static, {priv_dir, mstmnte, "webui"}}
    ].
