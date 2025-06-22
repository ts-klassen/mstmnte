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
    Opts = #{klsn_db => mstmnte:db_config(#{})},
    [
        %% Master endpoints (GET and POST share the same path).
        { <<"/mstmnte/master">>, mstmnte_route,
          {mstmnte_handler,
           #{ <<"GET">>  => list,
              <<"POST">> => bulk_get },
           Opts}},

        { <<"/mstmnte/master/:id">>, mstmnte_route, {mstmnte_handler, get, Opts}},

        %% Maintenance endpoints (GET and PATCH share the same path).
        { <<"/mstmnte/maintenance">>, mstmnte_route,
          {mstmnte_handler,
           #{ <<"GET">>   => maint_list,
              <<"PATCH">> => maint_patch },
           Opts}},

        { <<"/mstmnte/maintenance/:id">>, mstmnte_route, {mstmnte_handler, maint_get, Opts}}
    ].

%% Static assets route list.
-spec webui() -> routes().
webui() ->
    [ { <<"/mstmnte/webui/[...]">>, cowboy_static, {priv_dir, mstmnte, "webui"} } ].
