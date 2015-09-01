-module(fix_supersup).
-behaviour(supervisor).

-export([start_link/0, start_conn/2, stop_conn/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 4, 500}, []}}.

start_conn(Feed, Item) ->
    ChildSpec = {Feed, {fix_sup, start_link, [Feed, Item]}, permanent, 5000, supervisor, [fix_sup]},
    supervisor:start_child(?MODULE, ChildSpec).

stop_conn(Feed) ->
    supervisor:terminate_child(?MODULE, Feed),
    supervisor:delete_child(?MODULE, Feed).
