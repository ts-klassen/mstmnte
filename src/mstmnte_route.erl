-module(mstmnte_route).

%% ------------------------------------------------------------------
%% Generic Cowboy handler that simply delegates to a `{Module, Function}`
%% pair received as its *opts* argument. This avoids having to create one
%% tiny handler module per endpoint.
%%
%% Example route tuple for Cowboy 2.11:
%%   { '/mstmnte/master', mstmnte_route, {mstmnte_handler, list} }
%%
%% At runtime Cowboy will call init/2 which forwards the Req/State to
%% `Module:Function/2` (or /3 when extra opts are supplied).
%% ------------------------------------------------------------------

% Cowboy handler callbacks.

-export([init/2]).

-export_type([req/0, state/0]).

%% Optional additional options passed to the delegated handler.
-type extra() :: any().
-export_type([extra/0]).

-spec init(req(),
           {module(), atom()} | {module(), atom(), extra()} |
           {module(), map()}  | {module(), map(),  extra()}) ->
          {ok, req(), state()} | {stop, req()}.

-type req()    :: term().
-type state()  :: term().

%% Opts may be a 2-tuple {Module, Fun} or 3-tuple {Module, Fun, Extra} where
%% Extra becomes the 3rd argument when the target function supports it.
%% ------------------------------------------------------------------
%% The handler options passed in routes can now be either:
%%   1. {Module, Fun}                       – identical to the previous
%%      behaviour: always call Module:Fun/2.
%%   2. {Module, Fun, Extra}               – identical to the previous
%%      behaviour but passes Extra as third argument when supported.
%%   3. {Module, MethodsMap}               – where MethodsMap is a map()
%%      #{MethodBin => FunAtom}. The FunAtom corresponding to the
%%      request method (cowboy_req:method/1) will be invoked.
%%   4. {Module, MethodsMap, Extra}        – same as (3) but with an
%%      additional Extra argument passed to the chosen FunAtom when it
%%      supports arity 3.
%% ------------------------------------------------------------------

init(Req, {Module, Fun}) when is_atom(Fun) ->
    code:ensure_loaded(Module),
    Module:Fun(Req, undefined);
init(Req, {Module, Fun, Extra}) when is_atom(Fun) ->
    code:ensure_loaded(Module),
    case erlang:function_exported(Module, Fun, 3) of
        true  -> Module:Fun(Req, undefined, Extra);
        false -> Module:Fun(Req, undefined)
    end;

%% Map-based dispatch depending on HTTP method.
init(Req, {Module, MethodsMap}) when is_map(MethodsMap) ->
    handle_method_dispatch(Req, Module, MethodsMap, undefined);
init(Req, {Module, MethodsMap, Extra}) when is_map(MethodsMap) ->
    handle_method_dispatch(Req, Module, MethodsMap, Extra).

%% ------------------------------------------------------------------
%% Internal helpers
%% ------------------------------------------------------------------

-spec handle_method_dispatch(req(), module(), map(), extra() | undefined) -> {ok, req(), state()} | {stop, req()}.
handle_method_dispatch(Req0, Module, MethodsMap, Extra) ->
    Method = cowboy_req:method(Req0),
    case maps:get(Method, MethodsMap, undefined) of
        undefined ->
            %% HTTP method not allowed for this route.
            Req1 = cowboy_req:reply(405, Req0),
            {stop, Req1};
        Fun when is_atom(Fun) ->
            code:ensure_loaded(Module),
            case Extra of
                undefined -> Module:Fun(Req0, undefined);
                _ -> case erlang:function_exported(Module, Fun, 3) of
                         true  -> Module:Fun(Req0, undefined, Extra);
                         false -> Module:Fun(Req0, undefined)
                     end
            end
    end.
