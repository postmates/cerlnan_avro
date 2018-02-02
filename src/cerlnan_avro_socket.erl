-module(cerlnan_avro_socket).
-behavior(gen_server).

% API
-export([publish_blob/3]).

% gen_server callbacks
-export([start/1, start_link/1, init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-define(CERLNAN_AVRO_DEFAULT_BACKEND, cerlnan_avro_socket_v1).

%%====================================================================
%% Custom Types
%%====================================================================

-type avro_socket_backend() :: undefined | atom().
-type socket_args() :: #{
    backend => avro_socket_backend(),
    backend_args => map()
}.

-export_type([avro_socket_backend/0, socket_args/0]).

%%====================================================================
%% Behavior
%%====================================================================

-callback init(Args::map()) -> {ok, any()} | {error, any()}.
-callback publish_blob(Blob::iodata(), Args::map(), State::any()) -> {ok, any()} | {error, Reason::any()}.

%%====================================================================
%% API
%%====================================================================

-spec publish_blob(pid(), iodata(), map()) -> ok.
publish_blob(Socket, Blob, Args) ->
	gen_server:call(Socket, {publish_blob, Blob, Args}).

-spec start(socket_args()) -> {ok, pid()} | {error, term()}.
start(SocketArgs) ->
	gen_server:start(?MODULE, SocketArgs, []).

-spec start_link(socket_args()) -> {ok, pid()} | {error, term()}.
start_link(SocketArgs) ->
    gen_server:start_link(?MODULE, SocketArgs, []).

%%====================================================================
%% gen_server
%%====================================================================

-spec init(socket_args()) -> {ok, map()}.
init(SocketArgs) ->
    Backend =
        case maps:get(backend, SocketArgs, undefined) of
            undefined ->
                ?CERLNAN_AVRO_DEFAULT_BACKEND;
            Atom when is_atom(Atom) ->
                Atom
        end,
    BackendArgs = maps:get(backend_args, SocketArgs, #{}),
    {ok, BackendState} = Backend:init(BackendArgs),
    {ok, #{backend => Backend, backend_state => BackendState, socket => undefined}}.

handle_call({publish_blob, Blob, Args}, _From, State=#{backend:=Backend, backend_state:=BackendState}) ->
    {ok, NewBackendState} = Backend:publish_blob(Blob, Args, BackendState),
    {reply, ok, State#{backend_state=>NewBackendState}}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

code_change(_, _, State) ->
    {ok, State}.

terminate(_, _State) ->
    ok.

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

init_test() ->
    Args = #{
      backend => cerlnan_avro_socket_dummy,
      backend_args => #{}
    },
    {ok, #{}} = init(Args).

spawn_link_test() ->
    Args = #{
      backend => cerlnan_avro_socket_dummy,
      backend_args => #{}
    },
    {ok, Pid} = start_link(Args),
    exit(Pid, normal).

publish_blob_test() ->
    Args = #{
      backend => cerlnan_avro_socket_dummy,
      backend_args => #{}
    },
    {ok, Pid} = start_link(Args),
    ok = publish_blob(Pid, <<>>, #{}),
    exit(Pid, normal).

-endif.
