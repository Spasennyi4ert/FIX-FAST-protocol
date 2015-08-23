-module(fast_app).
-behaviour(application).

-export([start/2, stop/1]).

start(normal, _startArgs) ->
    {ok, Pid} = fast_supersup:start_link(),
    {ok, Conf} = file:consult("fast.config"),
    [application:set_env(fast, Conn, Data) || {Conn, Data} <- Conf],
    {ok, Pid}.

stop(_State) ->
    ok.
