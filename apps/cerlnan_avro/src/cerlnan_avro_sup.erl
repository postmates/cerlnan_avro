-module(cerlnan_avro_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->

    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    AvroPools = cerlnan_avro:pool_specs(),
    io:format("Starting: ~p~n", [AvroPools]),
    {ok, { {one_for_one, 5, 10}, AvroPools} }.

%%====================================================================
%% Internal functions
%%====================================================================
