-module(fast_segment).

-export(
   [
      decode/2
      ,decode_template_id/2
      ,decode_pmap/2
      ,decode_fields/2
   ]).

-include("../include/fast_template.hrl").
-include("../include/fast_context.hrl").
-include("../include/fast_common.hrl").

-import(fast_decode_types,
   [
      decode_pmap/1
      ,decode_type/3
   ]).


%% =========================================================================================================
%% decoding
%% =========================================================================================================

decode(Data, Context) ->
   try
      {Data1, Context1} = decode_pmap(Data, Context),
      {Data2, Context2} = decode_template_id(Data1, Context1),
      {Msg, Data3, Context3} = decode_fields(Data2, Context2),
      {ok, {Context3#context.template#template.name, Msg, Data3, Context3}}
   catch
      _:Err ->
         {error, Err}
   end.

decode_template_id(Data, Context = #context{dicts = Dicts, pmap = <<0:1, PMapRest/bits>>}) -> %
   case fast_dicts:get_value(global, ?common_template_id_key, Dicts) of
      undef ->
         throw({error, {'ERR D5', unable_to_know_template_id}});
      empty ->
         throw({error, {'ERR D6', unable_to_know_template_id}});
      Tid ->
         Template = fast_templates:get_by_id(Tid, Context#context.templates#templates.tlist),
         {Data, Context#context{pmap = PMapRest, template = Template}}
   end;

decode_template_id(Data,
   Context = #context{dicts = Dicts, pmap = <<1:1, PMapRest/bits>>, logger = L}) -> % tid is present into stream
   case decode_type(uInt32, Data, false) of
      {Tid, Err, Data1} ->
         L(Err, Tid),
         Template = fast_templates:get_by_id(Tid, Context#context.templates#templates.tlist),
         Dicts1 = fast_dicts:put_value(global, ?common_template_id_key, Tid, Dicts),
         {Data1, Context#context{pmap = PMapRest, template = Template, dicts = Dicts1}}
   end.

decode_pmap(Data, Context = #context{logger = L}) ->
   case fast_decode_types:decode_pmap(Data) of
      {Value, Err, Data1} ->
         L(Err, Value),
         {Data1, Context#context{pmap = Value}}
   end.

decode_fields(Data, Context = #context{template = #template{instructions = []}}) ->
   {[], Data, Context};

decode_fields(Data, Context = #context{template = Template = #template{instructions = [Instr | Tail]}}) ->
   {DecodedField, Data1, Context1} = fast_field_decode:decode(
      Data,
      Instr,
      Context#context{template = Template#template{instructions = Tail}}),
   {DecodedFields, Data2, Context2} = decode_fields(Data1, Context1),
   case DecodedField of
      skip ->
         {DecodedFields, Data2, Context2};
      {_FieldName, absent} ->
         {DecodedFields, Data2, Context2};
      DecodedField when is_list(DecodedField) ->
         {DecodedField ++ DecodedFields, Data2, Context2};
      DecodedField ->
         {[DecodedField | DecodedFields], Data2, Context2}
   end.


