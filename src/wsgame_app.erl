-module(wsgame_app).
-behaviour(application).

% Application callbacks
-export([start/2,stop/1]).

start(_Type, _StartArgs) ->
    % Start main application suprevisor
    Ret = wsgame_sup:start_link(),

    % Setup and start http cowboy server
    {ok, DocRoot} = application:get_env(docroot),
    {ok, Port} = application:get_env(port),
    {ok, Connections} = application:get_env(max_connections),
    Dispatch = 
	[
		{'_', [
			{[<<"game">>], wsgame_ws_handler, []},
                        {[<<"list">>], wsgame_list, []},
			{['...'], cowboy_static, [{directory, DocRoot}, {etag, default},
			    {mimetypes,[
				 {<<".html">>, [<<"text/html">>]},
                                 {<<".js">>, [<<"application/x-javascript">>]},
                                 {<<".css">>, [<<"text/css">>]},
                                 {<<".gif">>, [<<"image/gif">>]}
						  				   ]}]}
			]
		}
	],
    {ok, _} = cowboy:start_http(?MODULE, Connections, [{port, Port}], [{dispatch, Dispatch}]),
    Ret.

stop(_State) ->
    cowboy:stop_listener(?MODULE).
