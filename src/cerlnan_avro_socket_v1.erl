-module(cerlnan_avro_socket_v1).

-behaviour(cerlnan_avro_socket).

-export([init/1, publish_blob/3]).

-define(VERSION, 1).

-define(CONTROL_SYNC, 1).

%%====================================================================
%% Types
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
      sync => boolean(),
      shard_by => undefined | term()
    }.

-export_type([v1_init_args/0, v1_publish_args/0]).

%%====================================================================
%% API
%%====================================================================

-spec init(v1_init_args()) -> {ok, v1_state()}.
init(Args) ->
    Host = to_host(maps:get(host, Args, localhost)),
    Port = to_port(maps:get(port, Args, 2002)),
    ConnectTimeout = maps:get(connect_timeout, Args, 1000),
    ReadTimeout = maps:get(read_timeout, Args, 3000),
    SOpts = [binary, {active, false}, {packet, raw}],
    {ok, Sock} = gen_tcp:connect(Host, Port, SOpts, ConnectTimeout),
    {ok, #{socket=>Sock, read_timeout=>ReadTimeout}}.

-spec publish_blob(iodata(), v1_publish_args(), v1_state()) -> {ok | {error, term()}, v1_state()}.
publish_blob(Blob, Args, State=#{socket:=Sock, read_timeout:=ReadTimeout}) ->
    {Id, Payload} = payload(iolist_to_binary(Blob), Args),
    ok = gen_tcp:send(Sock, Payload),

    Res =
        case maps:get(sync, Args, true) of
            true ->
                wait_for_ack(Sock, Id, ReadTimeout);
            false ->
                ok
        end,
    {Res, State}.

%%====================================================================
%% Internal
%%====================================================================

to_host(Binary) when is_binary(Binary) ->
    binary_to_list(Binary);
to_host(Term) ->
    Term.

to_port(Binary) when is_binary(Binary) ->
    binary_to_integer(Binary);
to_port(List) when is_list(List) ->
    list_to_integer(List);
to_port(Int) ->
    Int.

header(ControlValue, IdValue, SortByValue) ->
    Version = <<?VERSION:4/big-unsigned-integer-unit:8>>,
    Control = <<ControlValue:4/big-unsigned-integer-unit:8>>,
    ID = <<IdValue:8/big-unsigned-integer-unit:8>>,
    SortBy = <<SortByValue:8/big-unsigned-integer-unit:8>>,
    <<Version/binary, Control/binary, ID/binary, SortBy/binary>>.

payload(Blob, Args) ->
    SortByDefault = rand:uniform(1 bsl 64),
    SortBy = erlang:phash2(maps:get(shard_by, Args, SortByDefault)),
    Id = rand:uniform(1 bsl 32),
    Control  =
        case maps:get(sync, Args, true) of
            false ->
                0;
            true ->
                ?CONTROL_SYNC
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

-ifdef(TEST).

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

payload_default_shard_by_unique_test() ->
    Args = #{
      sync => true
    },

    {_, [_, Payload]} = payload(<<>>, Args),
    {_, [_, Payload2]} = payload(<<>>, Args),

    <<_:4/binary, _:4/binary, _:8/binary, SortBy:8/binary, _/binary>> = Payload,
    <<_:4/binary, _:4/binary, _:8/binary, SortBy2:8/binary, _/binary>> = Payload2,

    ?assert(SortBy /= SortBy2).

payload_sync_by_default_test() ->
    SortBy = 10,
    Args = #{
      shard_by => SortBy
    },

    {_, [_, Payload]} = payload(<<>>, Args),
    <<?VERSION:4/big-unsigned-integer-unit:8, Payload2/binary>> = Payload,
    <<1:4/big-unsigned-integer-unit:8, Payload3/binary>> = Payload2,

    <<_Id:8/binary, Payload4/binary>> = Payload3,

    ExpectedSortBy = erlang:phash2(SortBy),
    <<ExpectedSortBy:8/big-unsigned-integer-unit:8, Payload5/binary>> = Payload4,
    <<>> = Payload5.

payload_async_test() ->
    SortBy = 10,
    Args = #{
      sync => false,
      shard_by => SortBy
    },

    {_, [_, Payload]} = payload(<<>>, Args),
    <<?VERSION:4/big-unsigned-integer-unit:8, Payload2/binary>> = Payload,
    <<0:4/big-unsigned-integer-unit:8, Payload3/binary>> = Payload2,

    <<_Id:8/binary, Payload4/binary>> = Payload3,

    ExpectedSortBy = erlang:phash2(SortBy),
    <<ExpectedSortBy:8/big-unsigned-integer-unit:8, Payload5/binary>> = Payload4,
    <<>> = Payload5.


publish_blob_echo_test() ->
    {EchoPid, EchoPort} = cerlnan_avro:echo_server(1),

    InitArgs = #{
        host => {127, 0, 0, 1},
        port => EchoPort
    },
    {ok, State=#{socket:=Socket}} = init(InitArgs),
    Blob = <<"1234567890">>,
    {ok, _} = publish_blob(Blob,  #{sync=>false}, State),

    {ok, Payload} = cerlnan_avro:recv_payload(Socket),
    <<_:4/binary, _:4/binary, _:4/binary, _:8/binary, _:8/binary, Blob/binary>> = Payload,
    exit(EchoPid, normal).

publish_blob_sync_happy_path_test() ->
    {AckPid, AckPort} = cerlnan_avro:ack_server(1),
    InitArgs = #{
        host => {127, 0, 0, 1},
        port => AckPort
    },
    {ok, State} = init(InitArgs),
    {ok, _} = publish_blob(<<>>, #{sync=>true}, State),
    exit(AckPid, normal).

publish_blob_sync_timeout_crashes_test() ->
    {AckPid, AckPort} = cerlnan_avro:timeout_server(1),
    InitArgs = #{
        host => {127, 0, 0, 1},
        port => AckPort,
        read_timeout => 500
    },
    {ok, State} = init(InitArgs),

    {{error, timeout}, _} = publish_blob(<<>>, #{sync=>true}, State),
    exit(AckPid, normal).

-endif.
