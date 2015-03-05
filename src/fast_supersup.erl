-module(fast_supersup).
-behavior(supervisor).

-export([start_link/0, stop/0, start_task/3, stop_task/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, fast}, ?MODULE, []).

stop() ->
    case whereis(fast) of
	P when is_pid(P) ->
	    exit(P, kill);
	_ ->
	    ok
    end.

init([]) ->
    {ok, {{one_for_one, 6, 3000}, []}}.

start_task(Name, Limit, MFA) ->
    ChildSpec = {Name,
		 {fast_sup, start_link, [Name, Limit, MFA]},
		 permanent, 10500, supervisor, [fast_sup]},
    supervisor:start_child(fast, ChildSpec).

stop_task(Name) ->
    supervisor:terminate_child(fast, Name),
    supervisor:delete_child(fast, Name).
