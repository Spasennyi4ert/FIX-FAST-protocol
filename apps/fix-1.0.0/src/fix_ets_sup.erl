-module(fix_ets_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

start_link(Name) ->
    supervisor:start_link({local, Name}, ?MODULE, [Name]).

init([Name]) ->
    {ok, {{one_for_one, 5, 3600},
     [{fix_mng, {fix_ets_mng, start_link, [Name]}, permanent, 5000, worker, [fix_ets_mng]}]}}.
