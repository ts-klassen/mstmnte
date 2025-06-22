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

-export([init/2]).

-type req()    :: term().
-type state()  :: term().

init(Req, {Module, Fun}) when is_atom(Module), is_atom(Fun) ->
    %% We forward an undefined State for now; the called function can
    %% ignore or overwrite it.
    Module:Fun(Req, undefined).
