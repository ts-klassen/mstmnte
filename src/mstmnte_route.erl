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

-spec init(req(), {module(), atom()} | {module(), atom(), extra()}) -> {ok, req(), state()}.

-type req()    :: term().
-type state()  :: term().

%% Opts may be a 2-tuple {Module, Fun} or 3-tuple {Module, Fun, Extra} where
%% Extra becomes the 3rd argument when the target function supports it.
init(Req, {Module, Fun}) ->
    code:ensure_loaded(Module),
    Module:Fun(Req, undefined);
init(Req, {Module, Fun, Extra}) ->
    code:ensure_loaded(Module),
    case erlang:function_exported(Module, Fun, 3) of
        true  -> Module:Fun(Req, undefined, Extra);
        false -> Module:Fun(Req, undefined)
    end.
