-module(cerlnan_avro_byte_buffer).
-behaviour(gen_server).

% An imposter for file descriptor, supporting only writes.
%
% Why?  erlavro's API currently only supports writing to files.

% External API
-export([open/0, close/1, close/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

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
    gen_server:start_link(?MODULE, ok, []).

-spec close(buffer()) -> {ok, iodata()} | {error, term()}.
close(Buffer) ->
    close(Buffer, 1000).

-spec close(buffer(), undefined | non_neg_integer()) -> {ok, iodata()} | {error, term()}.
close(Buffer, Timeout) ->
    gen_server:call(Buffer, close, Timeout).

%%====================================================================
%% GenServer callbacks
%%====================================================================

init(_) ->
    {ok, []}.

handle_call(close, _From, State) ->
    {stop, normal, {ok, State}, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info({io_request, From, ReplyAs, Request}, State) ->
    {Tag, Reply, NewState} = request(Request, State),
    true = Tag =:= ok orelse Tag =:= error,
    _ = reply(From, ReplyAs, Reply),
    {noreply, NewState}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
        {ok, State}.
%%====================================================================
%% Internal
%%====================================================================
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

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

open_close_test() ->
    {ok, Buffer} = open(),
    {ok, []} = close(Buffer).

ordering_test() ->
    {ok, Buffer} = open(),
    Writes = [<<"1">>, <<"23456">>, <<"789">>, <<"0">>],
    [ok = file:write(Buffer, Write) || Write <- Writes],

    WritesBin = iolist_to_binary(Writes),
    {ok, IoList} = close(Buffer),
    WritesBin = iolist_to_binary(IoList).

-endif.
