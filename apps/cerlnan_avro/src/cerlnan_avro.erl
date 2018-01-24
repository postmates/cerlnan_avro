-module(cerlnan_avro).
-export([child_spec/1, connect/1, connect/2, publish_blob/1, publish_blob/2, publish_blob/3]).

%%====================================================================
%% Custom Types
%%====================================================================
-type cerlnan_avro_backend() :: undefined | atom().
-opaque socket() :: pid().

-export_type([socket/0]).

%%====================================================================
%% API
%%====================================================================

child_spec(BackendArgs) ->
    Backend = application:get_env(?MODULE, backend, undefined),
    PoolSize = application:get_env(?MODULE, pool_size, 10),
    PoolOverflow = application:get_env(?MODULE, pool_overflow, 20),
    PoolArgs = [
        {name, {local, ?MODULE}},
        {worker_module, cerlnan_avro_socket},
        {size, PoolSize},
        {max_overflow, PoolOverflow}],
    SocketArgs = #{
        backend => Backend,
        backend_args => BackendArgs
    },
    poolboy:child_spec(?MODULE, PoolArgs, SocketArgs).

-spec publish_blob(binary()) -> ok.
publish_blob(Blob) ->
    poolboy:transaction(
        ?MODULE,
        fun(Socket) ->
            publish_blob(Socket, Blob)
        end).

-spec publish_blob(socket() | binary(), binary() | map()) -> ok | {error, term()}.
publish_blob(Socket, Blob) when is_pid(Socket) ->
    publish_blob(Socket, Blob, #{});
publish_blob(Blob, BackendArgs) ->
    poolboy:transaction(
        ?MODULE,
        fun(Socket) ->
            publish_blob(Socket, Blob, BackendArgs)
        end).

-spec publish_blob(socket(), binary(), map()) -> ok | {error, term()}.
publish_blob(Socket, Blob, Args) ->
	cerlnan_avro_socket:publish_blob(Socket, Blob, Args).

-spec connect(map()) -> socket().
connect(BackendArgs) when is_map(BackendArgs) ->
    connect(undefined, BackendArgs).

-spec connect(cerlnan_avro_backend(), map()) -> socket().
connect(Backend, BackendArgs) ->
    connect(Backend, BackendArgs).
