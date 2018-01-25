-module(cerlnan_avro_byte_buffer).

% An imposter for file descriptor, supporting only writes.
%
% Why?  erlavro's API currently only supports writing to files.

% External API
-export([open/0, close/1]).

% Internal API - exported out of necessity.
-export([init/0, loop/1]).

%%====================================================================
%% Custom Types
%%====================================================================

-opaque buffer() :: pid().

-export_type([buffer/0]).

%%====================================================================
%% API
%%====================================================================

-spec open() -> buffer().
open() ->
    start_link().

-spec close(buffer()) -> iodata().
close(Buffer) ->
    Buffer ! {close, self()},
    receive
        {Buffer, Bytes} ->
            Bytes
    end.

%%====================================================================
%% Internal
%%====================================================================

start_link() ->
    spawn_link(?MODULE,init,[]).

init() ->
    ?MODULE:loop([]).

loop(Bytes) ->
    receive
    {io_request, From, ReplyAs, Request} ->
        case request(Request,Bytes) of
            {Tag, Reply, NewBytes} when Tag =:= ok; Tag =:= error ->
                reply(From, ReplyAs, Reply),
                ?MODULE:loop(NewBytes)
        end;
    {close, From} ->
        From ! {self(), Bytes};
    _Unknown ->
        ?MODULE:loop(Bytes)
    end.

reply(From, ReplyAs, Reply) ->
    From ! {io_reply, ReplyAs, Reply}.

request({put_chars, Encoding, Chars}, Bytes) ->
    put_chars(unicode:characters_to_list(Chars,Encoding),Bytes);
request({put_chars, Encoding, Module, Function, Args}, Bytes) ->
    try
        request({put_chars, Encoding, apply(Module, Function, Args)}, Bytes)
    catch
    _:_ ->
        {error, {error,Function}, Bytes}
    end;
request({put_chars,Chars}, Bytes) ->
    request({put_chars,latin1,Chars}, Bytes);
request({put_chars,M,F,As}, Bytes) ->
    request({put_chars,latin1,M,F,As}, Bytes);
request(_, Bytes) ->
    {error, {error,enotsup}, Bytes}.

put_chars(Chars, []) ->
    {ok, ok, [Chars]};
put_chars(Chars, IoList) ->
    {ok, ok, [IoList, Chars]}.

%%====================================================================
%% Tests
%%====================================================================

-include_lib("eunit/include/eunit.hrl").

new_close_test() ->
    Buffer = open(),
    [] = close(Buffer).

ordering_test() ->
    Buffer = open(),
    Writes = [<<"1">>, <<"23456">>, <<"789">>, <<"0">>],
    [ok = file:write(Buffer, Write) || Write <- Writes],

    WritesBin = iolist_to_binary(Writes),
    WritesBin = iolist_to_binary(close(Buffer)).
