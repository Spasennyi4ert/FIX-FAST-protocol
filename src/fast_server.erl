-module(fast_server).
-behaviour(gen_server).

-compile(export_all).

-include("../include/fast_context.hrl").
-include("../include/fast_server.hrl").

start_link(Name) ->
	gen_server:start_link(?MODULE, Name, []).

stop() ->
	gen_server:call(?MODULE, stop).

connect(Pid, SendTo) ->
    gen_server:call(Pid, {connect, SendTo}).

root() ->
  case code:lib_dir(fast) of
    {error, _Error} -> filename:dirname(code:which(?MODULE)) ++ "/../";
    Root_ when is_list(Root_) -> Root_
  end.


init(Name) ->
%	ftrc:start(),
    fast:start_task(fast_disp, {fast_fsm_decode, start_link, []}),
    fast:run(fast_disp, [self()]),
    {ok, Options} = application:get_env(fast, Name),
    {source, Source} = lists:keyfind(source, 1, Options),
    {port, Port} = lists:keyfind(port, 1, Options),
    {group, Group} = lists:keyfind(group, 1, Options),
%	Source = "172.27.129.77",%
%	Port = "24027",%27
%	Group = "239.192.7.27",%27

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

    {ok, Pid} = gen_udp:controlling_process(Socket, self()),  
    {ok, #server{socket = Socket, name = Name, pid = Pid}}.

ip_to_binary(Ip) ->
	list_to_binary(tuple_to_list(Ip)).

handle_call({connect, SendTo}, _From, #server{}) ->
    Connected = #server{pid_fsm = SendTo},
    {noreply, SendTo, Connected};
handle_call(stop, _, P) ->
	{stop, shutdown, stopped, P}.

handle_info({udp, _Socket, _Ip, _Port, <<>>}, _) ->
	ok;
%handle_info({udp, _Socket, _Ip, _Port, << _Head:16, Bin/binary>>}, #server{name = Name}) ->
%	F = fun([], _) ->
%		ok;
%	(Err, Val) ->
%		io:format("~p ~p~n", [Err, Val])
%	end,
%	{ok, Context} = fast:create_context({file, root() ++"/spec/templates.xml"}, [], F),
%	case fast_segment:decode(Bin, Context) of
%		{ok, {TemplateName, Msg, _Rest, _}} ->
%			extract_date(TemplateName, Msg, Name),
%			io:format("~p ~p ~p~n", [TemplateName, Msg, Name]),
%			{noreply,#server{buffer = Bin, name = Name}};
%		{error, Reason} ->
%			io:format("ERROR: ~p, Bin = ~p ~n", [Reason, Bin]),
%			exit(failed)
%		
%	end;
handle_info({udp, _Socket, _Ip, _Port, Bin}, #server{name = Name, pid = Pid, pid_fsm = PidTo} = State) ->
    fast_fsm_decode:feed(PidTo, {Pid, Name, Bin}),
    {noreply, State};
	
handle_info(_Info, State) ->
	{noreply, State}.

%terminate(normal, State) ->
%	ftrc:stop().
%	{noreply, State}.
terminate(_,_) ->
	ok.

extract_date(TemplateName, Msg, Name) when TemplateName == <<"DefaultIncrementalRefreshMessage">> ->
	Trade = [Deal || {<<"MDEntries">>, Deal} <- Msg],
	deeg(Trade, Name);
%	io:format(" ~p~n ~n", [Trade]);
extract_date(TemplateName, Msg, Name) ->
	io:format("~p ~p ~p~n ~n", [TemplateName, Msg, Name]).

deeg([[H|T]], Name) when is_list(H) andalso T == [] ->
	filter(H, Name);
deeg([[H|T]], Name) when is_list(H) ->
%	io:format(" ~p~n ~n", [Trade]),
	filter(H, Name),
	deeg([T], Name).

filter(L, Name) ->
	case lists:member({<<"MDUpdateAction">>,0},L) of
		true ->
			case lists:member({<<"MDEntryType">>,<<"2">>},L) of
				true ->
					case lists:member({<<"SecurityID">>,72994630},L) of
						true ->
							Raw_Price = insert(L),%
							Price = remake(Raw_Price),
%							fast_index:get_data(Price),
							fast_ind:get_data(Price),
							io:format("~p ~p~n ~n", [Price, Name]);
						false ->
							{ok, nothing}
					end;
				false ->
					{ok, nothing}
			end;
		false ->
			{ok, nothing}
	end.

insert(L) ->
	[Price || {<<"MDEntryPx">>,Price} <- L].

remake([{Mantissa, Exponent}]) ->
	Price = Mantissa * math:pow(10,Exponent),
	Price.

feed({Name, Bin}) ->
    io:format("~p ~p~n ~n", [Bin, Name]).
