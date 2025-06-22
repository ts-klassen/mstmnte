-module(mstmnte_router).

-export([api/0, webui/0]).

%% ------------------------------------------------------------------
%% Public helpers that return route lists ready for cowboy_router:compile/1
%% ------------------------------------------------------------------

api() ->
    [
        %% Master endpoints
        {'GET',  <<"/mstmnte/master">>,          mstmnte_route, {mstmnte_handler, list}},
        {'POST', <<"/mstmnte/master">>,          mstmnte_route, {mstmnte_handler, bulk_get}},
        {'GET',  <<"/mstmnte/master/:id">>,      mstmnte_route, {mstmnte_handler, get}},

        %% Maintenance endpoints
        {'GET',  <<"/mstmnte/maintenance">>,     mstmnte_route, {mstmnte_handler, maint_list}},
        {'GET',  <<"/mstmnte/maintenance/:id">>, mstmnte_route, {mstmnte_handler, maint_get}},
        {'PATCH',<<"/mstmnte/maintenance">>,     mstmnte_route, {mstmnte_handler, maint_patch}}
    ].

webui() ->
    [
        {'GET', <<"/mstmnte/webui/[...]">>, cowboy_static, {priv_dir, mstmnte, "webui"}}
    ].
