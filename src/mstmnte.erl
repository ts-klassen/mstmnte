-module(mstmnte).

%% ------------------------------------------------------------------
%% Simple public helpers. Currently only init/0,1 which makes sure the
%% CouchDB database exists before the HTTP layer starts serving requests.
%% ------------------------------------------------------------------

-export([init/0, init/1, db_config/1]).
-export_type([opts/0]).

%% Options map passed around by the public API and HTTP handlers.
-type opts() :: #{klsn_db => mstmnte_db:config()}.

%% @doc Ensure the default (or configured) database exists.
-spec init() -> ok.
init() ->
    init(#{}).

%% @doc Ensure the database described by Opts exists.
-spec init(opts()) -> ok.
init(Opts) ->
    Config  = db_config(Opts),
    mstmnte_db:create_db(Config).

-spec db_config(opts()) -> mstmnte_db:config().
db_config(Opts) ->
    Default = #{db_name => <<"mstmnte">>, db_info => klsn_db:db_info()},
    AppDbInfo = application:get_env(mstmnte, klsn_db, #{}),
    maps:merge(Default, maps:get(klsn_db, Opts, AppDbInfo)).
