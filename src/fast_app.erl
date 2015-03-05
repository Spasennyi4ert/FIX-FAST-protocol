-module(fast_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(normal, _startArgs) ->

	{ok, Pid} = fast_supersup:start_link(),
	{ok, Conf} = file:consult("fast.config"),
	[application:set_env(fast, Conn, Data) || {Conn, Data} <- Conf],
	{ok, Pid}.

stop(_State) ->
    ok.
