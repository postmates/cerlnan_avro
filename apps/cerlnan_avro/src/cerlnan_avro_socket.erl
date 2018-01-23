-module(cerlnan_avro_socket).
-behaviour(gen_server).

% API
-export([publish_blob/3]).

% gen_server callbacks
-export([start/1, start/2, start_link/1, start_link/2, init/1, handle_call/3, handle_cast/2]).

%%====================================================================
%% Custom Types
%%====================================================================

-type avro_socket_backend() :: undefined | atom().

-export_type([avro_socket_backend/0]).

%%====================================================================
%% Behavior
%%====================================================================

-callback init(Args::map()) -> {ok, any()} | {error, any()}.
-callback publish_blob(Blob::binary(), Args::map(), State::any()) -> {ok, any()} | {error, Reason::any()}.

%%====================================================================
%% API
%%====================================================================

-spec publish_blob(pid(), binary(), map()) -> ok.
publish_blob(Socket, Blob, Args) ->
	gen_server:call(Socket, {publish_blob, Blob, Args}).

-spec start(map()) -> ok | {error, any()}.
start(BackendArgs) ->
    start(undefined, BackendArgs).

-spec start(avro_socket_backend(), map()) -> ok | {error, any()}.
start(Backend, BackendArgs) ->
	gen_server:start(?MODULE, {Backend, BackendArgs}, []).

-spec start_link(map()) -> {ok, pid()} | {error, any()}.
start_link(BackendArgs) ->
    start_link(undefined, BackendArgs).

-spec start_link(avro_socket_backend(), map()) -> {ok, pid()} | {error, any()}.
start_link(Backend, BackendArgs) ->
    gen_server:start_link(?MODULE, {Backend, BackendArgs}, []).

%%====================================================================
%% gen_server
%%====================================================================

-spec init({avro_socket_backend(), map()}) -> {ok, map()} | {error, any()}.
init({MaybeBackend, BackendArgs}) ->
    Backend =
        case MaybeBackend of
            undefined -> cerlnan_avro_socket_v1;
            Atom when is_atom(Atom) -> Atom
        end,
    case Backend:init(BackendArgs) of
        {ok, BackendState} ->
            {ok, #{backend => Backend, backend_state => BackendState, socket => undefined}};
        Error ->
            Error
    end.

handle_call({publish_blob, Blob, Args}, _From, State=#{backend:=Backend, backend_state:=BackendState}) ->
    {ok, NewBackendState} = Backend:publish_blob(Blob, Args, BackendState),
    {reply, ok, State#{backend_state=>NewBackendState}}.

handle_cast(_, State) ->
    {noreply, State}.
