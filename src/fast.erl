-module(fast).

-include("log.hrl").
-include("../include/business.hrl").

-compile(export_all).

start_task(Name, {M,F,A}) ->
    fast_supersup:start_task(Name, {M,F,A}).

stop_task(Name) ->
    fast_supersup:stop_task(Name).

run(Name, Args) ->
    fast_task_server:run(Name, Args).

now() ->
  timestamp(to_date_ms(erlang:now())).

timestamp({{YY,MM,DD},{H,M,S,Milli}}) ->
  % 20120529-10:40:17.578
  lists:flatten(io_lib:format("~4..0B~2..0B~2..0B-~2..0B:~2..0B:~2..0B.~3..0B", [YY, MM, DD, H, M, S, Milli])).

to_date_ms({Mega, Sec, Micro}) ->
  Seconds = Mega*1000000 + Sec,
  Milli = Micro div 1000,
  {Date, {H,M,S}} = calendar:gregorian_seconds_to_datetime(Seconds + calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}})),
  {Date, {H,M,S,Milli}}.

utc_ms() ->
  utc_ms(erlang:now()).

utc_ms({Mega, Sec, Micro}) ->
  (Mega*1000000+Sec)*1000 + Micro div 1000.

pack(MessageType, Body, SeqNum, Sender, Target) when 
MessageType =/= undefined, is_list(Body), is_integer(SeqNum), Sender =/= undefined, Target =/= undefined ->
  Header2 = [{msg_type, MessageType},{sender_comp_id, Sender}, {target_comp_id, Target}, {msg_seq_num, SeqNum}
  % ,{poss_dup_flag, "N"}
  ] ++ case proplists:get_value(sending_time, Body) of
    undefined -> [{sending_time, fast:now()}];
    _ -> []
  end,
  Body1 = encode(Header2 ++ Body),
  BodyLength = iolist_size(Body1),
  Body2 = iolist_to_binary([encode([{begin_string, "FIX.4.4"}, {body_length, BodyLength}]), Body1]),
  CheckSum = checksum(Body2),
  Body3 = [Body2, encode([{check_sum, CheckSum}])],
  %?D({out,Header2, dump(Body3)}),
	%io:format("~p ~n", [Header2]),
  Body3.

checksum(Packet) ->
  lists:flatten(io_lib:format("~3..0B", [lists:sum([Char || <<Char>> <=iolist_to_binary(encode(Packet))]) rem 256])).

encode(Packet) when is_binary(Packet) -> Packet;
encode([{_K,_V}|_] = Packet) ->
  [[fast_parser:number_by_field(Key), "=", fast_parser:encode_typed_field(Key, Value), 1] || {Key, Value} <- Packet].

encode_value(Value) when is_number(Value) -> integer_to_list(Value);
encode_value(Value) when is_float(Value) -> io_lib:format("~.2f", [Value]);
encode_value(Value) when is_list(Value) -> Value;
encode_value(Value) when is_binary(Value) -> Value.

dump(Bin) ->
  re:replace(iolist_to_binary(Bin), "\\001", "|", [{return,binary},global]).

decode_bin(Bin) ->
  try decode0(Bin) of
    Result -> Result
  catch
    error:Error ->
      ?DBG("Failed to decode fix '~s': ~p~n~p~n", [fast:dump(Bin), Error, erlang:get_stacktrace()]),
      error(invalid_fix)
  end.

decode0(Bin) ->
  case decode_fields(Bin) of
    {ok, Fields, MessageBin, Rest} ->
      {ok, fast_group:postprocess(fast_parser:decode_message(Fields)), MessageBin, Rest};
    Else ->
      Else
  end.

decode_fields(<<"8=FIX.4.4",1,"9=", Bin/binary>> = FullBin) ->
  case binary:split(Bin, <<1>>) of
    [BinLen, Rest1] ->
      BodyLength = list_to_integer(binary_to_list(BinLen)),
      case Rest1 of
        <<Message:BodyLength/binary, "10=", _CheckSum:3/binary, 1, Rest2/binary>> ->
          MessageLength = size(FullBin) - size(Rest2),
          <<MessageBin:MessageLength/binary, _/binary>> = FullBin,
          {ok, fast_splitter:split(Message), MessageBin, Rest2};
        _ ->
          {more, BodyLength + 3 + 3 + 1 - size(Rest1)}
      end;
    _ ->
      {more, 1}
  end;
decode_fields(<<"8", Rest/binary>>) when length(Rest) < 14 ->
  {more, 14 - size(Rest)};
decode_fields(<<"8", _/binary>>) ->
  {more, 1};
decode_fields(<<>>) ->
  {more, 14};
decode_fields(<<_/binary>>) ->
  error.

stock_to_instrument(Stock) when is_atom(Stock) ->
  stock_to_instrument(atom_to_binary(Stock,latin1));
stock_to_instrument(Stock) when is_binary(Stock) ->
  case binary:split(Stock, <<".">>, [global]) of
    [<<Currency1:3/binary, Currency2:3/binary>>] -> {undefined, <<Currency1/binary, "/", Currency2/binary>>};
    [Sym] -> {undefined, Sym};
    [<<"FX_", _/binary>> = Ex, <<Currency1:3/binary, Currency2:3/binary>>] -> {Ex, <<Currency1/binary, "/", Currency2/binary>>};
    [Ex, Sym] -> {Ex, Sym};
    [Ex, Sym, Date] -> {Ex, Sym, Date}
  end.

instrument_to_stock({undefined, <<Currency1:3/binary, "/", Currency2:3/binary>>}) ->
  binary_to_atom(<<Currency1/binary, Currency2/binary>>, latin1);
instrument_to_stock({<<"FX_", _/binary>> = Exchange, <<Currency1:3/binary, "/", Currency2:3/binary>>}) ->
  binary_to_atom(<<Exchange/binary, ".", Currency1/binary, Currency2/binary>>, latin1);
instrument_to_stock({Exchange, Symbol, Maturity}) when is_binary(Exchange) andalso is_binary(Symbol) 
  andalso is_binary(Maturity) ->
  binary_to_atom(<<Exchange/binary, ".", Symbol/binary, ".", Maturity/binary>>, latin1);
instrument_to_stock({Exchange, Symbol}) when is_binary(Exchange) andalso is_binary(Symbol) ->
  binary_to_atom(<<Exchange/binary, ".", Symbol/binary>>, latin1).

get_stock(#execution_report{market_segment_id = Market, symbol = Symbol}) ->
  instrument_to_stock({Market, Symbol}).

cfi_code(futures) -> "FXXXXX";
cfi_code(undefined) -> "MRCXXX";
cfi_code(<<"FX_TOD">>) -> "MRCXXX";
cfi_code(<<"FX_TOM">>) -> "MRCXXX";
cfi_code(_) -> "FXXXXX".

stock_to_instrument_block(Stock) ->
  case stock_to_instrument(Stock) of
    {Market, Symbol} ->
      [{symbol, Symbol}, {cfi_code, cfi_code(Market)}, {market_segment_id, Market}];
    {Market, Symbol, Date} ->
      [{symbol, Symbol}, {cfi_code, cfi_code(futures)}, {maturity_month_year, Date}, {market_segment_id, Market}]
  end.
