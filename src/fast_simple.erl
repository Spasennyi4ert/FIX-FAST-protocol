-module(fast_simple).
-compile(export_all).

root() ->
  case code:lib_dir(fast) of
    {error, _Error} -> filename:dirname(code:which(?MODULE)) ++ "/../";
    Root_ when is_list(Root_) -> Root_
  end.

decode(_Context, <<>>)->
   ok;
decode(Context, Binary) ->
	case erlang_fast:decode(Binary, Context) of
		{ok, {TemplateName, Msg, Rest, _}} ->
			io:format("~p ~p~n", [TemplateName, Msg]),
			decode(Context, Rest);
		{error, Reason} ->
			io:format("ERROR: ~p, Bin = ~p ~n", [Reason, Binary]),
			exit(failed)
	end.

start([MulticastSource, MulticastGroup, Port]) ->
	F = fun([], _) ->
		ok;
	(Err, Val) ->
		io:format("~p ~p~n", [Err, Val])
	end,
	{ok, Context} = fast:create_context({file, root() ++"/spec/templates.xml"}, [], F),
	{ok,SourceAddress} = inet:parse_address(MulticastSource),
	{ok,GroupAddress} = inet:parse_address(MulticastGroup),
		GroupIp = ip_to_binary(GroupAddress),
		LocalIp = ip_to_binary({0,0,0,0}),
		SourceIp = ip_to_binary(SourceAddress),
		Bin = << GroupIp/binary,LocalIp/binary,SourceIp/binary >>,
	{ok, Socket} = gen_udp:open(erlang:list_to_integer(Port),
      [
         inet,
         binary,
         {active, true},
         {reuseaddr, true},
	{multicast_ttl, 30},
         {raw, 0, 39, Bin}
      ]),
   Pid = spawn(fun() -> loop(Context, Socket) end),
   ok = gen_udp:controlling_process(Socket, Pid),
   receive
      _Msg ->
         exit(ok)
   end.

ip_to_binary(Ip) ->
	list_to_binary(tuple_to_list(Ip)).

loop(Context, Socket) ->
   receive
      {udp, _Socket, _SrcAddr, _Port, BinData} ->
         decode(Context,BinData),
         loop(Context, Socket)
   end.

