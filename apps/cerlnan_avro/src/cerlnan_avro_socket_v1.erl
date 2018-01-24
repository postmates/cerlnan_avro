-module(cerlnan_avro_socket_v1).

-behaviour(cerlnan_avro_socket).

-export([init/1, publish_blob/3]).

-define(VERSION, 1).

%%====================================================================
%% Custom Types
%%====================================================================

-type v1_state() :: gen_tcp:socket().

-type v1_init_args() ::
    #{
      host => atom() | string(),
      port => integer(),
      timeout => timeout()
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
    Timeout = maps:get(timeout, Args, 1000),
    SOpts = [binary, {active, false}, {packet, raw}],
    {ok, _} = gen_tcp:connect(Host, Port, SOpts, Timeout).

-spec publish_blob(binary(), v1_publish_args(), v1_state()) -> {ok, v1_state()}.
publish_blob(Blob, Args, Tcp) ->
    Header = header(Args),
    Payload = <<Header/binary, Blob/binary>>,
    Bytes = byte_size(Payload),
    Len = <<Bytes:4/big-unsigned-integer-unit:8>>,

    ok = gen_tcp:send(Tcp, [Len, Payload]),
    {ok, Tcp}.

%%====================================================================
%% Internal
%%====================================================================

header(Args) ->
    SortByDefault = rand:uniform(1 bsl 64),
    SortByValue = erlang:phash2(maps:get(sort_by, Args, SortByDefault)),
    {ControlValue, IDValue}  =
        case maps:get(id, Args, undefined) of
            undefined ->
                {0, 0};
            Term ->
                {1, erlang:phash2(Term)}
        end,

    Version = <<?VERSION:4/big-unsigned-integer-unit:8>>,
    Control = <<ControlValue:4/big-unsigned-integer-unit:8>>,
    ID = <<IDValue:8/big-unsigned-integer-unit:8>>,
    SortBy = <<SortByValue:8/big-unsigned-integer-unit:8>>,
    <<Version/binary, Control/binary, ID/binary, SortBy/binary>>.

%%====================================================================
%% Tests
%%====================================================================

-include_lib("eunit/include/eunit.hrl").

header_test() ->
    Id = 1,
    SortBy = 10,
    Args = #{
      id => Id,
      sort_by => SortBy
    },

    Header = header(Args),
    <<?VERSION:4/big-unsigned-integer-unit:8, Header2/binary>> = Header,
    <<1:4/big-unsigned-integer-unit:8, Header3/binary>> = Header2,

    ExpectedId = erlang:phash2(Id),
    <<ExpectedId:8/big-unsigned-integer-unit:8, Header4/binary>> = Header3,

    ExpectedSortBy = erlang:phash2(SortBy),
    <<ExpectedSortBy:8/big-unsigned-integer-unit:8, Header5/binary>> = Header4,
    <<>> = Header5.

header_default_sort_by_unique_test() ->
    Id = 1,
    Args = #{
      id => Id
    },

    Header = header(Args),
    Header2 = header(Args),

    <<_:4/binary, _:4/binary, _:8/binary, SortBy:8/binary, _/binary>> = Header,
    <<_:4/binary, _:4/binary, _:8/binary, SortBy2:8/binary, _/binary>> = Header2,

    ?assert(SortBy /= SortBy2).

header_async_by_default_test() ->
    SortBy = 10,
    Args = #{
      sort_by => SortBy
    },

    Header = header(Args),
    <<?VERSION:4/big-unsigned-integer-unit:8, Header2/binary>> = Header,
    <<0:4/big-unsigned-integer-unit:8, Header3/binary>> = Header2,

    <<_Id:8/binary, Header4/binary>> = Header3,

    ExpectedSortBy = erlang:phash2(SortBy),
    <<ExpectedSortBy:8/big-unsigned-integer-unit:8, Header5/binary>> = Header4,
    <<>> = Header5.

recv_payload(Sock) ->
    case gen_tcp:recv(Sock, 4) of
        {ok, LenBytes} ->
            <<Len:4/big-unsigned-integer-unit:8>> = LenBytes,
            {ok, Payload} = gen_tcp:recv(Sock, Len),
            {ok, <<LenBytes/binary, Payload/binary>>};

        Error ->
            Error
    end.

spawn_echo_once_server() ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {ip, {127,0,0,1}}, {packet, raw}, {active, false}]),
    {ok, Port} = inet:port(Listen),

    AcceptOnce =
        fun() ->
            {ok, Sock} = gen_tcp:accept(Listen),
            {ok, LenPrefixedPayload} = recv_payload(Sock),
            ok = gen_tcp:send(Sock, LenPrefixedPayload),
            ok = gen_tcp:close(Sock),
            ok = gen_tcp:close(Listen)
        end,


    Pid = spawn(AcceptOnce),
    {Pid, Port}.

publish_blob_test() ->
    {EchoPid, EchoPort} = spawn_echo_once_server(),

    InitArgs = #{
        host => {127, 0, 0, 1},
        port => EchoPort
    },
    {ok, Socket} = init(InitArgs),
    Blob = <<"1234567890">>,
    publish_blob(Blob, #{}, Socket),

    {ok, Payload} = recv_payload(Socket),
    <<_:4/binary, _:4/binary, _:4/binary, _:8/binary, _:8/binary, Blob/binary>> = Payload.
