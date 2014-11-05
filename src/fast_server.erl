-module(fast_server).
-behaviour(gen_server).

-compile(export_all).

-include("../include/fast_context.hrl").
-include("../include/fast_server.hrl").

start_link() ->
	gen_server:start_link(?MODULE, [], []).

root() ->
	case code:lib_dir(fast) of
		{error, _Error} ->
			filename:dirname(code:which(?MODULE)) ++ "/../";
			Root_ when is_list(Root_) ->
				Root_
	end.

init([]) ->
        ftrc:start(),
	
	Source = "172.27.129.77",
	Port = "24027",
	Group = "239.192.7.27",

	{ok,SourceAddress} = inet:parse_address(Source),
	{ok,GroupAddress} = inet:parse_address(Group),
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
    ok = gen_udp:controlling_process(Socket, self()),  
    {ok, #server{socket = Socket}}.

ip_to_binary(Ip) ->
	list_to_binary(tuple_to_list(Ip)).
handle_info({udp, _Socket, _Ip, _Port, <<>>}, _) ->
	ok;
handle_info({udp, _Socket, _Ip, _Port, <<_Head:16, Bin/binary>>}, #server{}) ->
	F = fun([], _) ->
    	ok;
    (Err, Val) ->
    	io:format("~p ~p~n", [Err, Val])
    	end,
    	{ok, Context} = fast:create_context({file, root() ++"/spec/templates.xml"}, [], F),
    	case fast_segment:decode(Bin, Context) of
    		{ok, {TemplateName, Msg, _Rest, _}} ->
    			extract_date(TemplateName, Msg),
    			{noreply, #server{buffer = Bin}};
    		{error, Reason} ->
    			io:format("Error: ~p, Bin = ~p ~n", [Reason, Bin]),
    			exit(failed)
handle_info(_Info, State) ->
	{noreply, State}.

terminate(normal, State) ->
        ftrc:stop(),
	{noreply, State}.

extract_date(TemplateName, Msg) when TemplateName == <<"DefaultIncrementalRefreshMessage">> ->
	Trage = [Deal || {<<"MDEntries">>, Deal} <- Msg],
	deeg(Trade);
extract_date(TemplateName, Msg) ->
	io:format("~p ~p~n ~n", [TemplateName, Msg]).

deeg([[H|T]]) when is_list(H) andalso T == [] ->
	filter(H);
deeg([[H|T]]) when is_list(H) ->
%	io:format(" ~p~n ~n", [Trade]),
	filter(H),
	deeg([T]).

filter(L) ->
	case lists:member({<<"MDUpdateAction">>,0},L) of
		true ->
			case lists:member({<<"MDEntryType">>,<<"2">>},L) of
				true ->
					case lists:member({<<"SecurityID">>,66489926},L) of
						true ->
							%insert(L);
							io:format("~p ~n ~n", [L]);
						false ->
							{ok, nothing}
					end;
				false ->
					{ok, nothing}
			end;
		false ->
			{ok, nothing}
	end.

