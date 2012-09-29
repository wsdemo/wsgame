-module(wsgame_list).
-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, _Opts) -> {ok, Req, undefined_state}.

handle(Req, State) ->
    {ok, Req2} = cowboy_req:reply(200, [], list_games(), Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->  ok.

list_games() -> 
    mochijson:encode({array, find(erlang:registered(), 0)}).

find([], _) -> [];
find(_, 10) -> [];
find([RegName | Tail], N) ->
    case atom_to_list(RegName) of 
        "game_" ++ UUID -> 
            [list_to_binary(UUID) | find(Tail, N+1)];
        _ -> 
            find(Tail, N)
    end.
