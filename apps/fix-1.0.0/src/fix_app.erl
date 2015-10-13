-module(fix_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(normal, _startArgs) ->
	{ok, Pid} = fix_supersup:start_link(),
	{ok, Pid}.

stop(_State) ->
    ok.
