-module(fix).
-behaviour(application).

-export([start/2, stop/1]).

start(normal, _Args) ->
  fix_sup:start_link().
  
stop(_) ->
  ok.
