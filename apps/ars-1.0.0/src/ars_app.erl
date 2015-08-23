-module(ars_app).
-behaviour(application).

-export([start/2, stop/1]).

start(normal, _startArgs) ->
    {ok, Pid} = ars_supersup:start_link(),
    {ok, Pid}.

stop(_State) ->
    ok.
