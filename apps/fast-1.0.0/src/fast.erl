-module(fast).

-export([start_server/2, stop_server/1, start_dispatcher/0, stop_dispatcher/0]).

start_server(Feed,SecID) ->
    fast_supersup:start_server(Feed,SecID).

stop_server(Feed) ->
    fast_supersup:stop_server(Feed).

start_dispatcher() ->
    fast_supersup:start_server().

stop_dispatcher() ->
    fast_supersup:stop_server().
