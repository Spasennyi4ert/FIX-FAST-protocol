-module(fast).
-behaviour(application).

-export([start/2, stop/1]).

start(normal, _Args) ->
  fast_sup:start_link().
  
stop(_) ->
  ok.
