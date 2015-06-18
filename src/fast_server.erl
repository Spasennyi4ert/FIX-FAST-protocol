-module(fast_server).
-behaviour(gen_server).

-compile(export_all).
-include_lib("stdlib/include/ms_transform.hrl").
-include("../include/fast_context.hrl").
-include("../include/fast_server.hrl").
-include("log.hrl").

-define(NTH, 5).
-define(ID, 10).
-define(SEQ, 7).
-define(SEQRPT, 7).
-define(EXP, 18).
-define(MANT, 19).
-define(STEP, 16).
-define(POINTX, 12).
-define(POINTY, 16).
-define(POINTZ, 17).

start_link(Name, SecID) ->
    gen_server:start_link(?MODULE, [Name, SecID], []).

stop() ->
    gen_server:call(?MODULE, stop).

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

handle_call(stop, _, P) ->
    {stop, shutdown, stopped, P};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({udp, _Socket, _Ip, _Port, <<>>}, State) ->
    {noreply, State};
handle_info({udp, _Socket, _Ip, _Port, Bin}, #server{name = Feed, sec_id = SecID} = State) ->
    List = lists:reverse(lists:foldl(fun parse/2, [<<>>], [<<C:8>> || <<C:8>> <= Bin])),
    if length(List) > 8 ->
	    case lists:nth(?ID, List) of
		SecID ->
		    case Feed of
			feed_a ->
			    %?D (List),
			    L = dig(List),
			    extract(L);
			    
			    %fast_server_decode:feed(T);
			feed_b ->
			    L = dig(List),
			    extract(L);
			    %fast_server_decode:feed(T);
			feed_c ->
			    T = list_to_tuple([lists:nth(X, List) || X <- [?SEQRPT, ?EXP, ?MANT]]),
			    fast_server_decode:feed(T);
			feed_d ->
			    T = list_to_tuple([lists:nth(X, List) || X <- [?SEQRPT, ?EXP, ?MANT]]),
			    fast_server_decode:feed(T);
			_ ->
			    ok
		    end;
		_ ->
		    ok
	    end;
       true ->
	    ok
    end,
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

dig(Bi) ->
	lists:zip3([lists:nth(X, Bi) || X <- lists:reverse(list_x(lists:nth(?SEQ,Bi),?POINTX))],
		    [lists:nth(Y, Bi) || Y <- lists:reverse(list(lists:nth(?SEQ,Bi),?POINTY))],
		    [lists:nth(Z, Bi) || Z <- lists:reverse(list(lists:nth(?SEQ,Bi),?POINTZ))]).

list_x(N,P) when N > 1 ->
    M = N - 1,
    [?STEP*(M)+P+1|list_x(N-1,P)];
list_x(N,P) when N == 1 ->
    M = N - 1,
    [?STEP*(M)+P|list_x(N-1,P)];
list_x(_N,_P) ->
    [].

list(N,P) when N >= 1 ->
    M = N - 1,
    [?STEP*(M)+P|list(N-1,P)];
list(_N,_P) ->
    [].

extract([H|T]) when is_tuple(H) andalso T == [] ->
    fast_server_decode:feed(H);
extract([H|T]) when is_tuple(H) ->
    %?D (H),
    fast_server_decode:feed(H),
    extract(T).
