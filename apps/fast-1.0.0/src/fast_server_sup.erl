-module(fast_server_sup).
-behaviour(supervisor).

-export([start_link/2, init/1]).

start_link(Feed, Item) ->
    supervisor:start_link({local, Feed}, ?MODULE, [Feed,Item]).

init([Feed,Item]) ->
    Supervisors = [{Feed, {fast_server, start_link, [Feed, Item]}, permanent, 5000, worker, [fast_server]}],
    {ok, {{one_for_one, 1, 3600}, Supervisors}}.
