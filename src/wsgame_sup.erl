-module(wsgame_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
    GamesSupervisor = 
	{ gamesup, {supervisor, start_link, [{local, gamesup}, ?MODULE, [games]]},
	  permanent, infinity, supervisor, []},
    Procs = [GamesSupervisor],
    {ok, {{one_for_one, 10, 60}, Procs}};

init([games]) ->
    {ok, {{one_for_one, 0, 10}, []}}. 
