-module(ars_task_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(MFA = {_, _, _}) ->
    supervisor:start_link(?MODULE, MFA).

init({M, F, A}) ->
    {ok, {{simple_one_for_one, 5, 3600},
	  [{ars_task, {M, F, A}, temporary, 5000, worker, [M]}]}}.
