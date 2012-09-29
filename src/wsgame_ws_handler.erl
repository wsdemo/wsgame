-module(wsgame_ws_handler).
-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3]).

-record(state, {
        gid,  % Game ID
        uid,  % Uer ID
        pid   % Game server PID
    }).   

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

% Initialize websocet connection
websocket_init(_TransportName, Req, _Opts) ->
    self() ! send_id,
    % Get game and user id that can be specified in URL
    GUID = get_val(<<"GUID">>, Req), UUID = get_val(<<"UUID">>, Req),
    % Connect to the game and set up a the handler state
    State = connect_ws_game(GUID, UUID),
    {ok, Req, State}.

% Handle websocket message
websocket_handle({text, Msg}, Req, State) ->
    % Make a move or crash (we expect only move messages)
    move(Msg, State),
    {ok , Req, State}.

% Handle erlang process-to-process message
websocket_info(send_id, Req, #state{gid = G, uid = U} = State) ->
    {reply, reply([{guid, G}, {uuid, U}]), Req, State};

websocket_info({update, List}, Req, State) ->
    {reply, reply(format(List, State)), Req, State}.

% Here is no clause for 'DOWN' message form game process 
% this process will crash receiving such message and
% socket will be closed. js should reconnect.

websocket_terminate(_Reason, _Req, _State) ->  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal functions
% Get value from URL
get_val(Key, Req) -> {Ret, _} = cowboy_req:qs_val(Key, Req), Ret.

% Create new game in both GUID and UUID are not defined
connect_ws_game(undefined , undefined) ->
    {GUID, {ok, _Pid}} = wsgame:create_game(),
    connect_ws_game(GUID, undefined);

% Join a game with GUID as a new user
connect_ws_game(GUID, undefined)  ->
    {UUID, Pid} = wsgame:join_game(GUID),
    monitor(process, Pid),
    #state{gid = GUID, uid = UUID, pid = Pid};

% Reconnect
connect_ws_game(GUID, UUID) ->
    Pid = wsgame:reconnect_game(GUID, UUID),
    monitor(process, Pid),
    #state{gid = GUID, uid = UUID, pid = Pid}.

% Commit move to game server
move(Msg, #state{uid = Uid, pid = Pid}) ->
    Move = {Uid, make_move(Msg)},
    gen_server:call(Pid, {move, Move}, infinity).

% Convert and filter move commands.
make_move(<<"up">>) -> {0,1};
make_move(<<"down">>) -> {0,-1};
make_move(<<"left">>) -> {-1,0};
make_move(<<"right">>) -> {1,0}.

% Json encoding related functions
reply(List) ->
    {text, mochijson:encode({struct, List})}.

format(Players, #state{uid = U}) ->
    Self = proplists:get_value(U, Players, dead),
    Enemy = orddict:erase(U, Players),
    {_, Cords} = lists:unzip(Enemy), 
    [{self, jxy(Self)}, {enemy, {array, [jxy(Pos) || Pos <- Cords]}}].
    
jxy(dead) -> dead;
jxy({X,Y}) -> {array, [X,Y]}.
