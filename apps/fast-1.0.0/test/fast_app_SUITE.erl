-module(fast_app_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([init_per_suite/1, end_per_suite/1, all/0]).

all() ->
    [].

init_per_suite(Config) ->
    application:start(fast),
    Config.

end_per_suite(_Config) ->
    application:stop(fast),
    ok.
