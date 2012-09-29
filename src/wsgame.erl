-module(wsgame).

%% API.
-export([start/0, makeid/0, regname/1, peer/1, peers/1]).
-export([create_game/0, join_game/1, reconnect_game/2]).

%% Connect to ther cluster and start application and deps.
start() ->
        net_adm:world(),
	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowboy),
	ok = application:start(wsgame).

% Simple destributed UUID generator
makeid() ->  base64:encode(term_to_binary({now(), node()})).
% UUID -> game_UUID atom
regname(UUID) -> list_to_atom("game_" ++ binary_to_list(UUID)).

% These functions are UUID-node mappings for spreading games across the cluster
% Because we have just two nodes it is simplified 
peer( _ ) -> hd(peers(node() ) -- [node()]).
peers( _ ) -> 
    {ok , Peers} = application:get_env(wsgame, nodes),  Peers.

create_game() ->
    GameID = wsgame:makeid(), 
    RegName = wsgame:regname(GameID),
    PeerNode = wsgame:peer(GameID),
    {GameID, supervisor:start_child(gamesup , 
        {GameID, {gen_server, start_link,
            [{local, RegName}, wsgame_gamesrv, [GameID , master, PeerNode], []]}, 
        temporary, brutal_kill, worker, []})}.

join_game(GameID) ->
    {NReps, _BadNodes} = 
        gen_server:multi_call(peers(GameID), regname(GameID), join, infinity),
    {_, Reps} = lists:unzip(NReps),
    proplists:get_value(ok, Reps, failed).

reconnect_game(GameID, UserID) ->
    {NReps, _BadNodes} = 
        gen_server:multi_call(peers(GameID), regname(GameID), {recon, UserID}, infinity),
   {_, Reps} = lists:unzip(NReps),
    proplists:get_value(ok, Reps, failed).
