-module(cerlnan).

-export([clients/0]).

clients() ->
   application:get_env(?MODULE, clients, []).
