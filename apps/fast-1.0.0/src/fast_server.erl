-module(fast_server).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, stop/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(server, {
	  socket,
	  name,
	  sec_id
}).

start_link(Name, SecID) ->
    gen_server:start_link(?MODULE, [Name, SecID], []).

stop(Name) ->
    gen_server:call(?MODULE, {stop, Name}).

init([Name, SecID]) ->
    {ok, Options} = application:get_env(fast, Name),
    {source, Source} = lists:keyfind(source, 1, Options),
    {port, Port} = lists:keyfind(port, 1, Options),
    {group, Group} = lists:keyfind(group, 1, Options),
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
    {ok, #server{socket = Socket, name = Name, sec_id = SecID}}.

handle_call({stop, _Name}, _, P) ->
    {stop, shutdown, stopped, P};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({udp, _Socket, _Ip, _Port, <<>>}, State) ->
    {noreply, State};
handle_info({udp, _Socket, _Ip, _Port, <<Num:32/unsigned-little-integer, Bin/binary>>}, State = #server{}) ->
    List = lists:reverse(lists:foldl(fun parse/2, [<<>>], [<<C:8>> || <<C:8>> <= Bin])),
    io:format("List : ~p ~p~n", [Num, List]),
    {noreply, State};   
	
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_, _) ->
    ok.

ip_to_binary(Ip) ->
	list_to_binary(tuple_to_list(Ip)).

parse(<<0:1, C:7>>, [Cur|Acc]) ->
    [<<0:1, Cur/binary, C:7>> | Acc];
parse(<<1:1, C:7>>, [Cur|Acc]) ->
    [<<>>, binary:decode_unsigned(<<0:1, Cur/binary, C:7>>) | Acc].

