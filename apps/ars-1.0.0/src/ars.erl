-module(ars).

-export([start_service/2, stop_service/1, run_task/2, stop_task/1]).

start_service(Name, {M,F,A}) ->
    ars_supersup:start_task(Name, {M,F,A}).

stop_service(Name) ->
    ars_supersup:stop_task(Name).

run_task(Name, Args) ->
    ars_server:run(Name, Args).

stop_task(Name) ->
    ars_server:stop(Name).
