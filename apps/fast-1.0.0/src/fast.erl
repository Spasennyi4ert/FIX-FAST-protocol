
-module(fast).

-export([start_server/2, stop_server/1, start_dispatcher/2, stop_dispatcher/1]).

start_server(Feed,Item) ->
    fast_supersup:start_server(Feed,Item).

stop_server(Feed) ->
    fast_supersup:stop_server(Feed).

start_dispatcher(Type, Item) ->
    fast_supersup:start_dispatcher(Type, Item).

stop_dispatcher(Type) ->
    fast_supersup:stop_dispatcher(Type).
