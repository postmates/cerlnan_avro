-module(cerlnan_app).

-behaviour(application).

-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    start_client_apps(),
    cerlnan_sup:start_link().

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

start_client_apps() ->
   Enabled = cerlnan:clients(),
   StartAll = fun(App) -> {ok, _} = application:ensure_all_started(App) end,
   lists:map(StartAll, Enabled).
