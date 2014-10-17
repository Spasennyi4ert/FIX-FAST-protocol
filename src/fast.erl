-module(fast).

-include("../include/fast_context.hrl").

-export
([
      create_context/3
      ,reset_context/1
      ,decode/2
      ,encode/3
   ]).

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

encode(TemplateId, Msg, Context) ->
   fast_segment:encode(TemplateId, Msg, Context).
