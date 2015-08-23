-module(ars_supersup).
-behavior(supervisor).

-export([start_link/0, stop/0, start_task/2, stop_task/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ars}, ?MODULE, []).

stop() ->
    case whereis(ars) of
	P when is_pid(P) ->
	    exit(P, kill);
	_ ->
	    ok
    end.

init([]) ->
    {ok, {{one_for_one, 6, 3600}, []}}.

start_task(Name, MFA) ->
    ChildSpec = {Name,
		 {ars_sup, start_link, [Name, MFA]},
		 permanent,
		 10500,
		 supervisor,
		 [ars_sup]},
    supervisor:start_child(ars, ChildSpec).

stop_task(Name) ->
    supervisor:terminate_child(ars, Name),
    supervisor:delete_child(ars, Name).
