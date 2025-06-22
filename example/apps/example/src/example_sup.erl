%%%-------------------------------------------------------------------
%% @doc example top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(example_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},

    ok = mstmnte:init(),

    Dispatch = cowboy_router:compile([
                 { '_', mstmnte_router:api() ++ mstmnte_router:webui() }
               ]),

    %% Start Cowboy listener under the supervisor so it gets restarted
    %% if it crashes. We use the simple_one_for_one style child spec.
    %% Allow the HTTP port to be configured through the application
    %% environment. Default to 8080 when no configuration is provided.
    Port = application:get_env(example, port, 8080),

    ChildSpecs = [
        #{ id       => http,
           start    => {cowboy, start_clear, [http, [{port, Port}], #{env => #{dispatch => Dispatch}}]},
           restart  => permanent,
           shutdown => 5000,
           type     => worker,
           modules  => [cowboy] }
    ],

    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
