-module(cerlnan_avro).
-export([child_spec/1, connect/1, publish_blob/1, publish_blob/2, publish_blob/3]).

child_spec(BackendArgs) ->
    PoolArgs = [
        {name, {local, ?MODULE}},
        {worker_module, cerlnan_avro_socket},
        {size, 10},
        {max_overflow, 20}],
    poolboy:child_spec(?MODULE, PoolArgs, BackendArgs).

publish_blob(Blob) ->
    poolboy:transaction(
        ?MODULE,
        fun(Socket) ->
            publish_blob(Socket, Blob)
        end).

publish_blob(Socket, Blob) when is_pid(Socket) ->
    publish_blob(Socket, Blob, #{});
publish_blob(Blob, BackendArgs) ->
    poolboy:transaction(
        ?MODULE,
        fun(Socket) ->
            publish_blob(Socket, Blob, BackendArgs)
        end).

publish_blob(Socket, Blob, Args) ->
	cerlnan_avro_socket:publish_blob(Socket, Blob, Args).

connect(BackendArgs) when is_map(BackendArgs) ->
    connect(undefined, BackendArgs).

connect(Backend, BackendArgs) ->
    connect(Backend, BackendArgs).
