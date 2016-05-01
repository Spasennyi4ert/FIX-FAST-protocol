-module(fast_dispatcher_sup).
-behaviour(supervisor).

-export([start_link/2, init/1]).

start_link(Type, Item) ->
    supervisor:start_link({local, Type}, ?MODULE, [Type, Item]).

init([Type, Item]) ->
    Child = [{Type, {fast_dispatcher, start_link, [Type, Item]}, permanent, 5000, worker, [fast_dispatcher]}],
    {ok, {{one_for_one, 1, 3600}, Child}}.
