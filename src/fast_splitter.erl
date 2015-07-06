-module(fast_splitter).

-on_load(init_nif/0).
-include("log.hrl").

-export([split/1, field_by_number/1, parse_date/1]).

init_nif() ->
  Path = filename:dirname(code:which(?MODULE)) ++ "/../priv",
  Load = erlang:load_nif(Path ++ "/fast_splitter", 0),
  case Load of
    ok -> ok;
    {error, {Reason,Text}} -> io:format("Load fast_splitter failed. ~p:~p~n", [Reason, Text])
  end,
  ok.

split(_Binary) ->
  erlang:error(not_implemented).

field_by_number(_Field) ->
  erlang:error(not_implemented).


-include_lib("eunit/include/eunit.hrl").


-spec to_i(binary() | string() | number()) -> number().
to_i(B) when is_binary(B) -> list_to_integer(binary_to_list(B));
to_i(L) when is_list(L) -> list_to_integer(L);
to_i(I) when is_number(I) -> I.

parse_date(Time) ->
  % <<"20120525-09:40:03.062">> or <<"20120525-09:40:03">>
  {match, [YY, MM, DD, H, M, S]} = re:run(Time, "(\\d{4})(\\d{2})(\\d{2})-(\\d{2}):(\\d{2}):([\\d\\.]+)", [{capture,all_but_first,list}]),
  Year = to_i(YY),
  Month = to_i(MM),
  Day = to_i(DD),
  Hour = to_i(H),
  Minute = to_i(M),
  {Second, MilliSecond} = if
    length(S) == 2 ->
      {to_i(S), 0};
    true ->
      [S0, S1, $. | MS] = S,
      {to_i([S0, S1]), to_i(MS)}
  end,

  {{Year, Month, Day}, {Hour, Minute, Second, MilliSecond}}.
