-module(cerlnan_avro).
-export([pools/0, pool_specs/0,
         publish/3, publish/4, publish/5,
         publish_blob/1, publish_blob/2, publish_blob/3
        ]).

-ifdef(TEST).
-export([timeout_server/1, ack_server/1, echo_server/1, recv_payload/1]).
-endif.

%%====================================================================
%% Custom Types
%%====================================================================

-opaque pool() :: atom().

-type native_avro_term() :: null
    | boolean()
    | integer()
    | float()
    | iolist()
    | [native_avro_term()]
    | [{cerlnan_avro_schema:name_raw(), native_avro_term()}].

-export_type([pool/0, native_avro_term/0]).

%%====================================================================
%% API
%%====================================================================

pools() ->
    application:get_env(?MODULE, pools, [default_pool()]).

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

-spec publish(binary(), cerlnan_avro_schema:type_or_name(), native_avro_term()) -> ok | {error, term()}.
publish(Type, Schema, Record) ->
    publish(?MODULE, Type, Schema, Record, #{}).

-spec publish(pool(), binary(), cerlnan_avro_schema:type_or_name(), native_avro_term()) -> ok | {error, term()}.
publish(Pool, Type, Schema, Record) when is_atom(Pool) ->
    publish(Pool, Type, Schema, Record, #{});
publish(Type, Schema, Record, Args) ->
    publish(?MODULE, Type, Schema, Record, Args).

-spec publish(pool(), binary(), cerlnan_avro_schema:type_or_name(), native_avro_term(), map()) -> ok | {error, term()}.
publish(Pool, Type, Schema, RecordOrRecords, Args) when is_list(RecordOrRecords) ->
    case serialize(Type, Schema, RecordOrRecords) of
        {ok, IOList} ->
            publish_blob(Pool, IOList, Args);

        Error ->
            Error
    end.

serialize(Type, Schema, RecordOrRecords) ->
    Meta = [],
    Header = avro_ocf:make_header(Schema, Meta),
    Encoder = avro:make_encoder(Schema, [{encoding, avro_binary}]),
    {ok, Buffer} = cerlnan_avro_byte_buffer:open(),
    try
        RecordBlock =
            case RecordOrRecords of
                Records = [H|_] when is_list(H) ->
                    [Encoder(Type, Record) || Record <- Records];
                Record ->
                    [Encoder(Type, Record)]
            end,
        ok = avro_ocf:write_header(Buffer, Header),
        ok = avro_ocf:append_file(Buffer, Header, RecordBlock),
        cerlnan_avro_byte_buffer:close(Buffer)
    catch
        Class:Reason ->
            % This is ugly, however erlavro likes to crash for
            % a number of reasons.  To ensure we always return,
            % we snag whatever goes wrong and wrap it into
            % a generic error type.
            _ = lager:error(
                "~nStacktrace:~s",
                [lager:pr_stacktrace(erlang:get_stacktrace(), {Class, Reason})]
             ),
            {error, {serialization_failed, Reason}}
    end.

-spec publish_blob(iodata()) -> ok | {error, term()}.
publish_blob(Blob) ->
    publish_blob(?MODULE, Blob, #{}).

-spec publish_blob(pool(), iodata()) -> ok | {error, term()}.
publish_blob(Pool, Blob) ->
    publish_blob(Pool, Blob, #{}).

-spec publish_blob(pool(), iodata(), map()) -> ok | {error, term()}.
publish_blob(Pool, Blob, Args) ->
    poolboy:transaction(
        Pool,
        fun(Socket) ->
            cerlnan_avro_socket:publish_blob(Socket, Blob, Args)
        end).


%%====================================================================
%% Internal
%%====================================================================

to_integer(Binary) when is_binary(Binary) ->
    binary_to_integer(Binary);
to_integer(List) when is_list(List) ->
    list_to_integer(List);
to_integer(Int) when is_integer(Int) ->
    Int.

default_pool() ->
    DefaultPoolSize = os:getenv("CERLNAN_AVRO_POOL_SIZE", "10"),
    DefaultPoolOverflow = os:getenv("CERLNAN_AVRO_POOL_OVERFLOW", "20"),
    DefaultHost = os:getenv("CERLNAN_AVRO_HOST", "localhost"),
    DefaultPort = os:getenv("CERLNAN_AVRO_PORT", "2002"),
    DefaultConnectTimeout = os:getenv("CERLNAN_AVRO_CONNECT_TIMEOUT", "1000"),
    DefaultReadTimeout = os:getenv("CERLNAN_AVRO_READ_TIMEOUT", "3000"),

    DefaultOptions =
        #{pool_size => to_integer(application:get_env(?MODULE, pool_size, DefaultPoolSize)),
          pool_overflow => to_integer(application:get_env(?MODULE, pool_overflow, DefaultPoolOverflow)),
          backend_args =>
            #{host => application:get_env(?MODULE, host, DefaultHost),
              port => to_integer(application:get_env(?MODULE, port, DefaultPort)),
              connect_timeout => to_integer(application:get_env(?MODULE, connect_timeout, DefaultConnectTimeout)),
              read_timeout => to_integer(application:get_env(?MODULE, read_timeout, DefaultReadTimeout))
             }
         },
    {?MODULE, DefaultOptions}.

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

ack_server(N) ->
    spawn_server(fun ack_server/2, N).

echo_server(N) ->
    spawn_server(fun echo_server/2, N).

timeout_server(N) ->
    spawn_server(fun timeout_server/2, N).

recv_payload(Sock) ->
    case gen_tcp:recv(Sock, 4) of
        {ok, LenBytes} ->
            <<Len:4/big-unsigned-integer-unit:8>> = LenBytes,
            {ok, Payload} = gen_tcp:recv(Sock, Len),
            {ok, <<LenBytes/binary, Payload/binary>>};

        Error ->
            Error
    end.

-spec echo_server(port(), undefined | non_neg_integer()) -> ok | no_return().
echo_server(Listen, 0) ->
    ok = gen_tcp:close(Listen);
echo_server(Listen, N) ->
    {ok, Sock} = gen_tcp:accept(Listen),
    {ok, LenPrefixedPayload} = recv_payload(Sock),
    ok = gen_tcp:send(Sock, LenPrefixedPayload),
    ok = gen_tcp:close(Sock),
    case N of
        undefined ->
            echo_server(Listen, N);
        _ ->
            echo_server(Listen, N-1)
    end.

-spec ack_server(port(), undefined | non_neg_integer()) -> ok | no_return().
ack_server(Listen, 0) ->
    ok = gen_tcp:close(Listen);
ack_server(Listen, N) ->
    {ok, Sock} = gen_tcp:accept(Listen),
    {ok, LenPrefixedPayload} = recv_payload(Sock),
    <<_:4/binary, _:4/binary, _:4/binary, Id:8/binary, _/binary>> = LenPrefixedPayload,
    ok = gen_tcp:send(Sock, Id),
    ok = gen_tcp:close(Sock),
    case N of
        undefined ->
            ack_server(Listen, N);
        _ ->
            ack_server(Listen, N-1)
    end.

-spec timeout_server(port(), undefined | non_neg_integer()) -> no_return().
timeout_server(Listen, 0) ->
    ok = gen_tcp:close(Listen);
timeout_server(Listen, N) ->
    {ok, _Sock} = gen_tcp:accept(Listen),
    timer:sleep(5000),
    case N of
        undefined ->
            timeout_server(Listen, N);
        _ ->
            timeout_server(Listen, N-1)
    end.

-spec spawn_server(function(), undefined | non_neg_integer()) -> {pid(), inet:port()}.
spawn_server(F, N) ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {ip, {127,0,0,1}}, {packet, raw}, {active, false}]),
    {ok, Port} = inet:port(Listen),

    Pid = spawn_link(fun() -> F(Listen, N) end),
    {Pid, Port}.

default_pool_test() ->
    {?MODULE, PoolConfig} = default_pool(),
    #{pool_size := 10,
      pool_overflow := 20,
      backend_args := BackendArgs} = PoolConfig,
    #{host := "localhost",
      port := 2002,
      connect_timeout := 1000,
      read_timeout := 3000} = BackendArgs.

%%% Seriliazation Tests
cerlnan_avro_serialization_test_() ->
    {setup,
        fun () ->
            Pools = [{?MODULE, #{backend => cerlnan_avro_socket_dummy}}],
            application:set_env(?MODULE, pools, Pools),
            {ok, _Apps} = application:ensure_all_started(cerlnan_avro),
            ok
        end,
        fun(_Apps) ->
            application:unset_env(?MODULE, pools),
            application:stop(cerlnan_avro)
        end,
        [fun() -> ok = publish_blob_basic() end,
         fun() -> ok = publish_basic() end,
         fun() -> {error, {serialization_failed, _}} = publish_bad_value() end]
    }.

publish_blob_basic() ->
    publish_blob(<<>>).

publish_basic() ->
    publish_basic(true).

publish_basic(Sync) ->
    UserSchema =
        cerlnan_avro_schema:record(
            <<"User">>,
            [cerlnan_avro_schema:field(name, string),
             cerlnan_avro_schema:field(favorite_number, int),
             cerlnan_avro_schema:field(favorite_color, string)],
            [{namespace, 'example.avro'}]),
    User1 = [{name, "Foo Bar"}, {favorite_number, 10}, {favorite_color, "maroon"}],
    User2 = [{name, "Alice Bob"}, {favorite_number, 32}, {favorite_color, "greenish-gold"}],
    publish("example.avro.User", UserSchema, [User1, User2], #{sync=>Sync}).

publish_bad_value() ->
    UserSchema =
        cerlnan_avro_schema:record(
            <<"User">>,
            [cerlnan_avro_schema:field(name, string),
             cerlnan_avro_schema:field(favorite_number, int),
             cerlnan_avro_schema:field(favorite_color, string)],
            [{namespace, 'example.avro'}]),
    User1 = [{name, "Foo Bar"}, {favourite_number, 10}, {favourite_color, "maroon"}],
    {error, {serialization_failed, _}} = publish("example.avro.User", UserSchema, [User1]).

cerlnan_avro_timeout_test_() ->
    {setup,
        fun () ->
            {Pid, Port} = cerlnan_avro:timeout_server(undefined),
            application:set_env(?MODULE, port, Port),
            application:set_env(?MODULE, pool_size, 1),
            application:set_env(?MODULE, pool_overflow, 0),

            timer:sleep(1),
            {ok, _Apps} = application:ensure_all_started(cerlnan_avro),
            Pid
        end,
        fun(ServerPid) ->
            application:stop(cerlnan_avro),
            erlang:exit(ServerPid, normal)
        end,
        [fun() -> {error, timeout} = publish_basic() end,
         fun() -> ok = publish_basic(false) end
        ]
    }.

-endif.
