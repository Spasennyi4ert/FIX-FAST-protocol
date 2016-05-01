-module(fast_supersup).
-behaviour(supervisor).

-export([start_link/0, start_dispatcher/2, stop_dispatcher/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 4, 500}, []}}.

start_dispatcher(Type, Item) ->
    ChildSpec = {Type, {fast_dispatcher_sup, start_link, [Type, Item]}, permanent, 5000, supervisor, [fast_dispatcher_sup]},
    supervisor:start_child(?MODULE, ChildSpec).

stop_dispatcher(Type) ->
    supervisor:terminate_child(?MODULE, Type),
    supervisor:delete_child(?MODULE, Type).
