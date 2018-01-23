-module(cerlnan_avro_socket_v1).

-behaviour(cerlnan_avro_socket).

-export([init/1, publish_blob/3]).

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
    SOpts = [binary, {packet, 4}],
    {ok, _} = gen_tcp:connect(Host, Port, SOpts, Timeout).

-spec publish_blob(binary(), v1_publish_args(), v1_state()) -> {ok, v1_state()}.
publish_blob(_Blob, _Args, TCP) ->
    {ok, TCP}.
