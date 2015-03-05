-module(fast).

-include("../include/fast_context.hrl").

-export([start_task/3, stop_task/1, run/2]).
-export([ create_context/3,reset_context/1,decode/2]).

start_task(Name, Limit, {M,F,A}) ->
    fast_supersup:start_task(Name, Limit, {M,F,A}).

stop_task(Name) ->
    fast_supersup:stop(Name).

run(Name, Args) ->
    fast_task_server:run(Name, Args).

create_context(TemplatesDescr, Options, Logger) ->
   try
      {Dicts, Templates} = fast_xml:parse(TemplatesDescr, Options),
      {ok, #context{dicts = Dicts, templates = Templates, logger = Logger, options = Options}}
   catch
      _:Err ->
         Err
   end.
   
reset_context(Context = #context{dicts = Dicts}) ->
   Dicts1 = gb_trees:map(fun(_K, _V) -> [] end, Dicts),
   {ok, Context#context{dicts = Dicts1}}.

decode(Data, Context) ->
   fast_segment:decode(Data, Context).

