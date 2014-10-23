-module(decode_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

all() ->
    [{group, all}].

groups() ->
    [
     {all, [], [
                fast_server,
                fast_sample
               ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

fast_sample(Config) ->
    Context = build_context(root()),
    %% ct:pal("context: ~p", [Context]),
    ok = decode1(Config, Context, "d1"),
    ok = decode1(Config, Context, "d2"),
    ok = decode1(Config, Context, "d3"),
    ok.

fast_server(Config) ->
    Context = build_context(root()),
    %% ct:pal("context: ~p", [Context]),
    ok = decode2(Config, Context, "d1"),
    ok = decode2(Config, Context, "d2"),
    ok = decode2(Config, Context, "d3"),
    ok.

build_context(Root) ->
    F = fun([], _) ->
                ok;
           (Err, Val) ->
                io:format("~p ~p~n", [Err, Val])
        end,
    Path = filename:join([Root, "spec", "templates.xml"]),
    {ok, Context} = fast:create_context({file, Path}, [], F),
    Context.

root() ->
    TestDir = filename:dirname(code:which(?MODULE)),
    filename:join(TestDir, "..").

decode1(Config, Context, File) ->
    Dir = ?config(data_dir, Config),
    Path = filename:join([Dir, File]),
    %% ct:pal("path: ~p", [Path]),
    {ok, Bin} = file:read_file(Path),
    ok = fast_sample:decode(Context, Bin),
    ok.

decode2(Config, Context, File) ->
    Dir = ?config(data_dir, Config),
    Path = filename:join([Dir, File]),
    %% ct:pal("path: ~p", [Path]),
    {ok, Bin} = file:read_file(Path),
    fast_server:decode(Bin, Context),
    ok.

