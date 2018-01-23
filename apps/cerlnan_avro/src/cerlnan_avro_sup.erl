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
    PoolArgs = #{
        host => 'localhost',
        port => 2002
     },
    AvroPool = cerlnan_avro:child_spec(PoolArgs),
    {ok, { {one_for_one, 5, 10}, [AvroPool]} }.

%%====================================================================
%% Internal functions
%%====================================================================
