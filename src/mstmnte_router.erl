-module(mstmnte_router).

-export([api/0, webui/0]).

%% ------------------------------------------------------------------
%% Public helpers that return route lists ready for cowboy_router:compile/1
%% ------------------------------------------------------------------

-type route()  :: tuple().
-type routes() :: [route()].

-export_type([route/0, routes/0]).

-spec api() -> routes().
api() ->
    Opts = #{klsn_db => db_opts()},
    [
        %% Master endpoints
        { <<"/mstmnte/master">>,          [{method, <<"GET">>}],  mstmnte_route, {mstmnte_handler, list,      Opts}},
        { <<"/mstmnte/master">>,          [{method, <<"POST">>}], mstmnte_route, {mstmnte_handler, bulk_get,  Opts}},
        { <<"/mstmnte/master/:id">>,      [{method, <<"GET">>}],  mstmnte_route, {mstmnte_handler, get,       Opts}},

        %% Maintenance endpoints
        { <<"/mstmnte/maintenance">>,     [{method, <<"GET">>}],  mstmnte_route, {mstmnte_handler, maint_list,  Opts}},
        { <<"/mstmnte/maintenance/:id">>, [{method, <<"GET">>}],  mstmnte_route, {mstmnte_handler, maint_get,   Opts}},
        { <<"/mstmnte/maintenance">>,     [{method, <<"PATCH">>}], mstmnte_route, {mstmnte_handler, maint_patch, Opts}}
    ].

-spec db_opts() -> mstmnte_db:config().
db_opts() ->
    User = application:get_env(mstmnte, klsn_db, #{}),
    maps:merge(#{db_name => <<"mstmnte">>, db_info => #{}}, User).

%% Static assets route list.
-spec webui() -> routes().
webui() ->
    [ { <<"/mstmnte/webui/[...]">>, cowboy_static, {priv_dir, mstmnte, "webui"} } ].
