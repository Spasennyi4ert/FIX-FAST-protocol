%%% call start/0 from the suitable place. Call stop/0 from the suitable place,
%%% like terminate.
%%% Convert binary trace to text:
%%% run from console
%%% erl -eval 'dbg:trace_client(file, "xxx")' > xxx.txt
%%% wait till xxx.txt stops growing, then type in the console:
%%% init:stop().
%%% And start staring into the trace

-module(ftrc).

-export([
         start/0,
         stop/0
        ]).

start() ->
    Base = get_name(),
    Size = 102400000,
    F = dbg:trace_port(file, {Base, wrap, ".trc", Size}),
    dbg:tracer(port, F),
    dbg:p(all, [c]),
    Match = get_match(),
    Mods = get_modules(),
    [dbg:tpl(X, Match) || X <- Mods].

stop() ->
    dbg:ctpl(),
    dbg:flush_trace_port(),
    dbg:stop_clear().

get_modules() ->
    [
     fast_app,
     fast_decode_types,
     fast_dicts,
     fast_field_decode,
     fast_sample,
     fast_segment,
     fast_server,
     fast_sup,
     fast_templates,
     fast_utils,
     fast_xml,
     fast_xml_utils
    ].

get_name() ->
    {A, B, _} = now(),
    N = A * 1000000 + B,
    Name = lists:flatten(io_lib:format("~p-~B", [?MODULE, N])),
    filename:join("/tmp", Name).

get_match() ->
    %% or do from the erlang shell something complex, like this:
    %% dbg:fun2ms(fun([["Packet \"Id\": " | _],_,_]) ->
    %%                    true;
    %%               (_) ->
    %%                    message(false)
    %%            end).
    %% and write the resulting matching here:
    [{'_',[],[true,{return_trace}]}].


