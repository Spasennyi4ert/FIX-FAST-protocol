-module(fast_dispatcher_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, dispatch}, ?MODULE, []).

init([]) ->
    Child = {dispatch, {fast_dispatcher, start_link, []}, permanent, 5000, worker, [fast_dispatcher]},
    {ok, {{one_for_one, 1, 3600}, Child}}.
