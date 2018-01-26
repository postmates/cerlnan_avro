-module(cerlnan_avro).
-export([pools/0, pool_specs/0,
         publish/3, publish/4, publish/5,
         publish_blob/1, publish_blob/2, publish_blob/3
        ]).

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
    Meta = [],
    Header = avro_ocf:make_header(Schema, Meta),
    Encoder = avro:make_encoder(Schema, [{encoding, avro_binary}]),
    Buffer = cerlnan_avro_byte_buffer:open(),

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
        {ok, IoList} = cerlnan_avro_byte_buffer:close(Buffer),
        publish_blob(Pool, IoList, Args)
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

-spec publish_blob(iodata()) -> ok.
publish_blob(Blob) ->
    publish_blob(?MODULE, Blob, #{}).

-spec publish_blob(pool(), iodata()) -> ok.
publish_blob(Pool, Blob) ->
    publish_blob(Pool, Blob, #{}).

-spec publish_blob(pool(), iodata(), map()) -> ok.
publish_blob(Pool, Blob, Args) ->
    poolboy:transaction(
        Pool,
        fun(Socket) ->
            cerlnan_avro_socket:publish_blob(Socket, Blob, Args)
        end).

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

cerlnan_avro_test_() ->
    {setup,
        fun () ->
            Pools = [{?MODULE, #{backend => cerlnan_avro_socket_dummy}}],
            application:set_env(?MODULE, pools, Pools),
            {ok, _Apps} = application:ensure_all_started(cerlnan_avro),
            ok
        end,
        fun(_Apps) ->
            application:stop(cerlnan_avro)
        end,
        [fun publish_blob_basic/0,
         fun publish_basic/0,
         fun publish_bad_value/0]
    }.

publish_blob_basic() ->
    ok = publish_blob(<<>>).

publish_basic() ->
    UserSchema =
        cerlnan_avro_schema:record(
            <<"User">>,
            [cerlnan_avro_schema:field(name, string),
             cerlnan_avro_schema:field(favorite_number, int),
             cerlnan_avro_schema:field(favorite_color, string)],
            [{namespace, 'example.avro'}]),
    User1 = [{name, "Foo Bar"}, {favorite_number, 10}, {favorite_color, "maroon"}],
    User2 = [{name, "Alice Bob"}, {favorite_number, 32}, {favorite_color, "greenish-gold"}],
    ok = publish("example.avro.User", UserSchema, [User1, User2]).

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
-endif.
