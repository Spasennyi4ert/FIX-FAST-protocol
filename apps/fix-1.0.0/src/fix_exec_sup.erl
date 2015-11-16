-module(fix_exec_sup).
-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

start_link(Feed, Item) ->
    supervisor:start_link({local, Feed}, ?MODULE, [Feed, Item]).

init([Feed, Item]) ->
    {ok, {{one_for_one, 5, 3600},
     [{fix_conn, {fix_exec_conn, start_link, [Feed, Item]}, permanent, 5000, worker, [fix_exec_conn]}]}}.
