-module(mstmnte).

%% ------------------------------------------------------------------
%% Simple public helpers. Currently only init/0,1 which makes sure the
%% CouchDB database exists before the HTTP layer starts serving requests.
%% ------------------------------------------------------------------

-export([init/0, init/1]).
-export_type([opts/0]).

%% Options map passed around by the public API and HTTP handlers.
-type opts() :: #{klsn_db := mstmnte_db:config()}.

%% @doc Ensure the default (or configured) database exists.
-spec init() -> ok.
init() ->
    Opts = application:get_env(mstmnte, klsn_db, #{}),
    init(Opts).

%% @doc Ensure the database described by Opts exists.
-spec init(opts()) -> ok.
init(#{klsn_db := DbCfg0}) when is_map(DbCfg0) ->
    Default = #{db_name => <<"mstmnte">>, db_info => #{}},
    Config  = maps:merge(Default, DbCfg0),
    mstmnte_db:create_db(Config);

%% Guard against accidental misuse.
init(Other) ->
    error({invalid_opts, Other}).
