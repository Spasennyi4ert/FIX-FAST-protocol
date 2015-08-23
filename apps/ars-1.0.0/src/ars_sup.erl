-module(ars_sup).
-behavior(supervisor).

-export([start_link/2, init/1]).

start_link(Name, MFA) ->
    supervisor:start_link(?MODULE, {Name, MFA}).

init({Name, MFA}) ->
    {ok, {{one_for_all, 1, 3600},
	  [{serv,
	    {ars_server, start_link, [Name, self(), MFA]},
	    permanent,
	    5000,
	    worker,
	    [ars_server]}]}}.
