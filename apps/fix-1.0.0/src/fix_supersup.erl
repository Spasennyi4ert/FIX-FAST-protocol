-module(fix_supersup).
-behaviour(supervisor).

-export([start_link/0, table/1, conn/2, stop/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 4, 500}, []}}.

table(Name) ->
    ChildSpec = {Name, {fix_ets_sup, start_link, [Name]}, permanent, 5000, supervisor, [fix_ets_sup]},
    supervisor:start_child(?MODULE, ChildSpec).

conn(Feed, Item) ->
    ChildSpec = {Feed, {fix_exec_sup, start_link, [Feed, Item]}, permanent, 5000, supervisor, [fix_exec_sup]},
    supervisor:start_child(?MODULE, ChildSpec).

stop(Feed) ->
    supervisor:terminate_child(?MODULE, Feed),
    supervisor:delete_child(?MODULE, Feed).
