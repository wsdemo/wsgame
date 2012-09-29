-module(wsgame_gamesrv).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
      gameid,			 % Gameid
      pids = orddict:new(),      % Current Pid <-> player ID mapping
      role = master, 		 % Master or slave role
      peer_node, 		 % Name of node to run a peer process
      peer_pid,		 	 % Pid of the peer node	 
      players = [], 		 % List of active players
      inactive_since = now() % Last plaer quit timestamp
}).

init([Gameid, master, PeerNode]) ->  
    self() ! check_inactive,
    {ok, start_peer(#state{gameid = Gameid, peer_node = PeerNode})};

init([Gameid, slave, PeerNode, PeerPid]) ->
    monitor(process, PeerPid),
    {ok, #state{peer_node = PeerNode, peer_pid = PeerPid, 
            gameid = Gameid, role = slave}}.

% Gen_server  'call' handlers 
% Slave role
handle_call({sync, Players}, {Pid, _}, #state{role = slave, peer_pid = Pid} = State) ->
    {reply, ok, State#state{players = Players}};
handle_call( _ , _, #state{role = slave} = State) ->
    {reply, {error, slave_role}, State};

% Master role
handle_call(join, {Pid, _}, #state{pids = Pids, players = PL} = State) ->
    Id = wsgame:makeid(),
    {_, NewPL} = put_p({Id, {15,15}}, PL),
    NewPids = connect({Id, Pid}, Pids),
    NewState = State#state{pids = NewPids, players = NewPL},
    {reply, {ok, {Id, self()}}, send(check_players(sync(NewState)))};    

handle_call({recon, Id}, {Pid, _}, #state{pids = Pids} = State) ->
    NewPids = connect({Id, Pid}, stop_old_client(Id, Pids)),
    {reply, {ok, self()}, send(check_players(State#state{pids = NewPids}))};

handle_call({move, {Id, Dxy}}, {_, _}, #state{players = PL} = State) ->
    case orddict:find(Id, PL) of
        {ok, _} ->
            {_, NewPL} = put_p(move({Id, Dxy}, get_p(Id, PL)), PL),
            NewState = State#state{players = NewPL},
            {reply, ok, send(check_players(sync(NewState)))};

        _ -> {reply, {error, dead_id}, State}
    end;

handle_call(_Request, _From, State) ->
  {reply, {error, not_implemented} , State}.

% Ignore any casts
handle_cast(_Msg, State) -> {noreply, State}. 

% Ignore all other messages
% Down from a peer monitor. Become or stay a master, remove a peer pid.
handle_info({'DOWN',_,_, Pid,_}, #state{peer_pid = Pid} = State) ->
    timer:send_after(30 * 1000, reconnect),
    {noreply, check_players(State#state{role = master, peer_pid = undefined})};

% Possible DOWN form client. Remove it from the active client list.
handle_info({'DOWN',_,_, Pid,_}, #state{pids = Pids} = State) ->
     NewPids = lists:keydelete(Pid, 2, Pids),
     {noreply, check_players(State#state{pids = NewPids})};

% We get notification that there were no players in this game a minute ago.
% Check that game was realy inactive for more that 30 sec and close it in such case.
handle_info(check_inactive, #state{inactive_since = undefined} = State) ->
    {noreply, State};
handle_info(check_inactive, #state{inactive_since = Ts} = State) ->
    case timer:now_diff(os:timestamp(), Ts) > (30 * 1000 * 1000) of % 30 sec
        true -> {stop, normal, State};
        false -> {noreply, State} 
    end;

handle_info(reconnect, #state{peer_pid = undefined} = State) ->
    {noreply, sync(start_peer(State))};
handle_info(reconnect,  State) ->
    {noreply, State};

handle_info(_Info, State) -> {noreply, State}.


terminate(_Reason, _State) ->  ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal functions

start_peer( #state{peer_node = PeerNode, gameid = Gid} = State) -> 
    % Kill old process 
    case lists:member(PeerNode, nodes()) of
        true ->
            RegName = wsgame:regname(Gid),
            Resp = supervisor:start_child({gamesup, PeerNode} , 
                            {Gid, {gen_server, start_link, [
                                        {local, RegName}, wsgame_gamesrv, [Gid , slave, node(), self()], 
                                        [wsgame_gamesrv]]}, 
                        temporary, brutal_kill, worker, []}),
            case Resp of
                {ok, Pid} ->
                    monitor(process, Pid),
                    State#state{peer_pid = Pid};
                _ -> 
                   timer:send_after(10 * 1000, reconnect),
                   State
            end;
        false -> 
            timer:send_after(10 * 1000, reconnect),
            State
    end.

% Sync with slave
sync(#state{peer_pid = Pid, players = Players} = State) ->
    catch gen_server:call(Pid, {sync, Players}, infinity),
    State.

% Check if there is still someone here
check_players(#state{pids = [], inactive_since = undefined} = State) ->
    timer:send_after(60 * 1000, check_inactive),
    State#state{inactive_since = os:timestamp()};

check_players(#state{pids = []} = State) -> State;
check_players(State) -> State#state{inactive_since = undefined}.

% Send updates to clients
send(#state{pids = Pids, players = PL} = State) ->
    {_ , RawPids} = lists:unzip(Pids), [ Pid ! {update, PL} || Pid <- RawPids],
    State.

% Connect client
connect({Id, Pid}, Pids) ->
    monitor(process, Pid),
    orddict:store(Id, Pid, Pids).

% Disconnect old client
stop_old_client(Id, Pids) ->
    stop(proplists:get_value(Id, Pids), reconect),
    orddict:erase(Id, Pids). 

stop(undefined, _) -> ok;
stop(Pid, Reason) when is_pid(Pid) -> exit(Pid, Reason).

%
% Move related functions
%

% Get player in  {ID, {X, Y}} format
get_p(Id, Players) -> proplists:lookup(Id, Players).

% Perform a move
move({ID, {Dx, Dy}}, {ID, {X, Y}}) -> 
    {ID, check_range({X+Dx, Y+Dy}, {X,Y})}.

% Check if first argument is in range or return second one
check_range({X, Y} , _) when X>=0,X<30,Y>=0,Y<30 -> {X, Y};
check_range(_, {X, Y}) -> {X,Y}.
    
% Apply new player coordinates and find if someone is dead
put_p({ID, Pos}, Players) ->
    Dead = [ DeadID || {DeadID, DeadPos} <- Players, DeadPos==Pos, ID=/=DeadID],
    {Dead, orddict:store(ID, Pos, remove_dead(Dead, Players))}.

% Remove all dead players from list 
remove_dead([], Players) -> Players;
remove_dead([Dead | Tail] , Players) -> 
    remove_dead(Tail,  orddict:erase(Dead, Players)). 
