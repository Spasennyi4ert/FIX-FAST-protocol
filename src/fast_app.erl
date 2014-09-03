-module(fast_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _startArgs) ->
%	case file:path_consult("fast.conf", ["."]) of
%		{ok, Env, _} ->
%			application:set_env(fast, config, Env);
%		_ ->
%			ok
%	end,
	{ok, Pid} = fast_sup:start_link(),
	{ok, Conf} = file:consult("fast.conf"),
	[application:set_env(fast, Conn, Data) || {Conn, Data} <- Conf],
	{ok, Pid}.

stop(_State) ->
    ok.
