-module(cerlnan_avro_socket_dummy).

-behaviour(cerlnan_avro_socket).

-export([init/1, publish_blob/3]).

init(Map) when is_map(Map) ->
    {ok, #{}}.

publish_blob(Blob, Args, State) when is_binary(Blob) ; is_list(Blob), is_map(State) ->
    {ok, State}.
