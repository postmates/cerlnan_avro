-module(cerlnan_avro_socket_v1).

-behaviour(cerlnan_avro_socket).

-export([init/1, publish_blob/3]).

-define(VERSION, 1).

-define(CONTROL_SYNC, 1).

%%====================================================================
%% Custom Types
%%====================================================================

-type v1_state() ::
    #{
      read_timeout => timeout(),
      socket => gen_tcp:socket()
     }.

-type v1_init_args() ::
    #{
      host => inet:socket_address() | inet:hostname(),
      port => inet:port_number(),
      connect_timeout => timeout(),
      read_timeout => timeout()
    }.

-type v1_publish_args() ::
    #{
      id => undefined | term(),
      sort_by => undefined | term()
    }.

-export_type([v1_init_args/0, v1_publish_args/0]).

%%====================================================================
%% API
%%====================================================================

-spec init(v1_init_args()) -> {ok, v1_state()}.
init(Args) ->
    Host = maps:get(host, Args, localhost),
    Port = maps:get(port, Args, 2002),
    ConnectTimeout = maps:get(connect_timeout, Args, 1000),
    ReadTimeout = maps:get(read_timeout, Args, 3000),
    SOpts = [binary, {active, false}, {packet, raw}],
    {ok, Sock} = gen_tcp:connect(Host, Port, SOpts, ConnectTimeout),
    {ok, #{socket=>Sock, read_timeout=>ReadTimeout}}.

-spec publish_blob(iodata(), v1_publish_args(), v1_state()) -> {ok, v1_state()}.
publish_blob(Blob, Args, State=#{socket:=Sock, read_timeout:=ReadTimeout}) ->
    {Id, Payload} = payload(Blob, Args),
    ok = gen_tcp:send(Sock, Payload),

    case maps:is_key(id, Args) of
        true ->
            ok = wait_for_ack(Sock, Id, ReadTimeout);
        false ->
            ok
    end,
    {ok, State}.

%%====================================================================
%% Internal
%%====================================================================

header(ControlValue, IdValue, SortByValue) ->
    Version = <<?VERSION:4/big-unsigned-integer-unit:8>>,
    Control = <<ControlValue:4/big-unsigned-integer-unit:8>>,
    ID = <<IdValue:8/big-unsigned-integer-unit:8>>,
    SortBy = <<SortByValue:8/big-unsigned-integer-unit:8>>,
    <<Version/binary, Control/binary, ID/binary, SortBy/binary>>.

payload(Blob, Args) ->
    SortByDefault = rand:uniform(1 bsl 64),
    SortBy = erlang:phash2(maps:get(sort_by, Args, SortByDefault)),
    {Control, Id}  =
        case maps:get(id, Args, undefined) of
            undefined ->
                {0, rand:uniform(1 bsl 32)};
            Term ->
                {?CONTROL_SYNC, erlang:phash2(Term)}
        end,

    Header = header(Control, Id, SortBy),
    Payload = <<Header/binary, Blob/binary>>,
    Bytes = byte_size(Payload),
    Len = <<Bytes:4/big-unsigned-integer-unit:8>>,
    {Id, [Len, Payload]}.

wait_for_ack(Sock, Id, ReadTimeout) ->
    case gen_tcp:recv(Sock, 8, ReadTimeout) of
        {ok, <<Id:8/big-unsigned-integer-unit:8>>} ->
            ok;

        {ok, SomeOtherId} ->
            {error, {bad_ack, SomeOtherId}};

        Error ->
            Error
    end.

%%====================================================================
%% Tests
%%====================================================================

-include_lib("eunit/include/eunit.hrl").

header_test() ->
    Id = 1,
    SortBy = 10,

    Header = header(?CONTROL_SYNC, Id, SortBy),
    <<?VERSION:4/big-unsigned-integer-unit:8, Header2/binary>> = Header,
    <<?CONTROL_SYNC:4/big-unsigned-integer-unit:8, Header3/binary>> = Header2,

    <<Id:8/big-unsigned-integer-unit:8, Header4/binary>> = Header3,
    <<SortBy:8/big-unsigned-integer-unit:8, Header5/binary>> = Header4,
    <<>> = Header5.

payload_default_sort_by_unique_test() ->
    Id = 1,
    Args = #{
      id => Id
    },

    {_, [_, Payload]} = payload(<<>>, Args),
    {_, [_, Payload2]} = payload(<<>>, Args),

    <<_:4/binary, _:4/binary, _:8/binary, SortBy:8/binary, _/binary>> = Payload,
    <<_:4/binary, _:4/binary, _:8/binary, SortBy2:8/binary, _/binary>> = Payload2,

    ?assert(SortBy /= SortBy2).

payload_async_by_default_test() ->
    SortBy = 10,
    Args = #{
      sort_by => SortBy
    },

    {_, [_, Payload]} = payload(<<>>, Args),
    <<?VERSION:4/big-unsigned-integer-unit:8, Payload2/binary>> = Payload,
    <<0:4/big-unsigned-integer-unit:8, Payload3/binary>> = Payload2,

    <<_Id:8/binary, Payload4/binary>> = Payload3,

    ExpectedSortBy = erlang:phash2(SortBy),
    <<ExpectedSortBy:8/big-unsigned-integer-unit:8, Payload5/binary>> = Payload4,
    <<>> = Payload5.

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
    {ok, Sock} = gen_tcp:accept(Listen),
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

publish_blob_echo_test() ->
    {EchoPid, EchoPort} = spawn_server(fun echo_server/2, 1),

    InitArgs = #{
        host => {127, 0, 0, 1},
        port => EchoPort
    },
    {ok, State=#{socket:=Socket}} = init(InitArgs),
    Blob = <<"1234567890">>,
    publish_blob(Blob, #{}, State),

    {ok, Payload} = recv_payload(Socket),
    <<_:4/binary, _:4/binary, _:4/binary, _:8/binary, _:8/binary, Blob/binary>> = Payload,
    exit(EchoPid, normal).

publish_blob_sync_happy_path_test() ->
    {AckPid, AckPort} = spawn_server(fun ack_server/2, 1),
    Id = 1,
    InitArgs = #{
        host => {127, 0, 0, 1},
        port => AckPort
    },
    {ok, State} = init(InitArgs),
    {ok, _} = publish_blob(<<>>, #{id=>Id}, State),
    exit(AckPid, normal).

publish_blob_sync_timeout_crashes_test() ->
    {AckPid, AckPort} = spawn_server(fun timeout_server/2, 1),
    Id = 1,
    InitArgs = #{
        host => {127, 0, 0, 1},
        port => AckPort,
        read_timeout => 500
    },
    {ok, State} = init(InitArgs),

    try publish_blob(<<>>, #{id=>Id}, State) of
        {ok, _} ->
            ?assert(false)
    catch
        error:{badmatch, {error, timeout}} ->
            ok
    end,
    exit(AckPid, normal).
