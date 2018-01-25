-module(cerlnan_avro).
-export([pools/0, pool_specs/0, publish_blob/1, publish_blob/2, publish_blob/3]).

%%====================================================================
%% Custom Types
%%====================================================================
-type cerlnan_avro_backend() :: undefined | atom().
-opaque pool() :: atom().

-export_type([pool/0]).

%%====================================================================
%% API
%%====================================================================

pools() ->
    DefaultPool = {?MODULE, #{}},
    application:get_env(?MODULE, pools, [DefaultPool]).

pool_specs() ->
    Pools = pools(),
    [child_spec(PoolId, PoolArgs) || {PoolId, PoolArgs} <- Pools].

child_spec(PoolId, PoolArgs) when is_atom(child_spec), is_map(PoolArgs) ->
    Backend = maps:get(backend, PoolArgs, undefined),
    BackendArgs = maps:get(backend_args, PoolArgs, #{}),
    PoolSize = maps:get(pool_size, PoolArgs, 10),
    PoolOverflow = maps:get(pool_overflow, PoolArgs, 20),

    PoolboyArgs = [
        {name, {local, PoolId}},
        {worker_module, cerlnan_avro_socket},
        {size, PoolSize},
        {max_overflow, PoolOverflow}],
    SocketArgs = #{
        backend => Backend,
        backend_args => BackendArgs
    },
    poolboy:child_spec(?MODULE, PoolboyArgs, SocketArgs).

-spec publish_blob(binary()) -> ok.
publish_blob(Blob) ->
    publish_blob(?MODULE, Blob, #{}).

-spec publish_blob(pool(), binary()) -> ok.
publish_blob(Pool, Blob) ->
    publish_blob(Pool, Blob, #{}).

-spec publish_blob(pool(), binary(), map()) -> ok.
publish_blob(Pool, Blob, Args) ->
    poolboy:transaction(
        Pool,
        fun(Socket) ->
            cerlnan_avro_socket:publish_blob(Socket, Blob, Args)
        end).
