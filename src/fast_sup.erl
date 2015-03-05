-module(fast_sup).
-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

start_link(Name, Limit, MFA) ->
    supervisor:start_link(?MODULE, {Name, Limit, MFA}).


init({Name, Limit, MFA}) ->
    {ok, {{one_for_all, 1, 3600},
     [{serv,
       {fast_task_server, start_link, [Name, Limit, self(), MFA]},
		permanent,
		5000,
		worker,
		[fast__task_server]}]} }.
