-module(fast_server).
-behaviour(gen_server).

-compile(export_all).

-record(state, {
	socket,
	context
}).
-record(server, {
	buffer
}).




start_link() ->
	gen_server:start_link(?MODULE, [], []).

init([]) ->
	
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
    {ok, Pid} = fast:start_link(self()),
    ok = gen_udp:controlling_process(Socket, Pid),  %% 35
	{ok, #state{socket = Socket}}.

ip_to_binary(Ip) ->
	list_to_binary(tuple_to_list(Ip)).


handle_info(_Info, State) ->
	{noreply, State}.

terminate(normal, State) ->
	{noreply, State}.

