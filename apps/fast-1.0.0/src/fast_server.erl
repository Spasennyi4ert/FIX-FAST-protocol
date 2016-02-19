-module(fast_server).
-behaviour(gen_server).

-export([start_link/2, stop/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([init/1]).

-define(NOENTR, 6).
-define(ACT, 7).
-define(TYPE, 8).
-define(SECID, 9).
-define(RPTSEQ, 13).
-define(EXP, 17).
-define(MANT, 18).
-define(STEPB, 23).
-define(STEPS, 22).
-define(TIMER, 5000).

-record(server, {
	  socket,
	  feed,
	  sec_id,
	  time
}).

start_link(Feed, SecID) ->
    gen_server:start_link(?MODULE, [Feed, SecID], []).

stop(Feed) ->
    gen_server:call(?MODULE, {stop, Feed}).

init([Feed, SecID]) ->
    {ok, Options} = application:get_env(fast, Feed),
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
    erlang:send_after(?TIMER, self(), gc),
    {ok, #server{socket = Socket, sec_id = SecID}}.

handle_call({stop, _Feed}, _, P) ->
    {stop, shutdown, stopped, P};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({udp, _Socket, _Ip, _Port, <<>>}, State) ->
    {noreply, State};
handle_info({udp, _Socket, _Ip, _Port, <<_Num:32/unsigned-little-integer, Bin/binary>>}, S = #server{sec_id = SecID}) ->
%    List = lists:reverse(lists:foldl(fun parse/2, [<<>>], [<<C:8>> || <<C:8>> <= Bin])),
    List_binaries = list_binaries(Bin),
    List = sort(List_binaries),
    case lists:nth(2, List) of
	1 ->
	    L = template_inc(List),
	    F = lists:filter(fun({_, _, Id, _, _, _}) ->
				     Id =:= SecID end, L),
%	    case F of
%		[_] ->
%		    io:format("F : ~p~n", [F]),
		    extract(F);
%		[] ->
%		    ok
%	    end;
	2->
	    case lists:nth(9, List) of
		SecID ->
		    L = template_snap(List),
%		    case L of
%			[_] ->
			    extract(L);
%			[] ->
%			    ok
%		    end;
%	    Mantissa = lists:nth(17, List),
%	    Exponenta = lists:nth(18, List),
%	    MDEntryPx = {Mantissa, Exponenta},
%	    io:format("MDEntryPx : ~p~n", [MDEntryPx]);
		_ ->
		    ok
	    end;
%	6 ->
%	    MsgSeqNum = lists:nth(3, List),
%	    io:format("HeartBeat_MsgSeqNum : ~p~n", [MsgSeqNum]);
%	7 ->
%	    MsgSeqNum = lists:nth(3, List),
%	    io:format("SequenceReset_MsgSeqNum : ~p~n", [MsgSeqNum]);
%	8 ->
%	    MsgSeqNum = lists:nth(3, List),
%	    io:format("TradingSessionStatus_MsgSeqNum : ~p~n", [MsgSeqNum]);
	_ ->
	    ok
    end,
    {noreply, S};   
handle_info(gc, S) ->
    erlang:garbage_collect(self()),
    erlang:send_after(?TIMER, self(), gc),
    {noreply, S};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_, _) ->
    ok.

ip_to_binary(Ip) ->
	list_to_binary(tuple_to_list(Ip)).

%parse(<<0:1, C:7>>, [Cur|Acc]) ->
%    [<<0:1, Cur/binary, C:7>> | Acc];
%parse(<<1:1, C:7>>, [Cur|Acc]) ->
%    [<<>>, binary:decode_unsigned(<<0:1, Cur/binary, C:7>>) | Acc].

list_binaries(Bin) ->
    lists:reverse(list_binaries_acc(Bin, [])).

list_binaries_acc(<<>>, Acc) ->
    Acc;
list_binaries_acc(<<C:8, Rest/binary>>, Acc) ->
    list_binaries_acc(Rest, [<<C:8>>|Acc]).

sort(List) ->
    lists:reverse(sort_acc(List, [<<>>])).

sort_acc([], Acc) ->
    Acc;
sort_acc([<<0:1, C:7>>|T], [Cur|Acc]) ->
    sort_acc(T, [<<0:1, Cur/binary, C:7>> | Acc]);
sort_acc([<<1:1, C:7>>|T], [Cur|Acc]) ->
    sort_acc(T, [<<>>, binary:decode_unsigned(<<0:1, Cur/binary, C:7>>) | Acc]).

template_inc(List) ->
    zip6([lists:nth(A, List) || A <- list_inc(?ACT,lists:nth(?NOENTR,List),lists:nth(?TYPE,List),List,?TYPE)],
	 [lists:nth(T, List) || T <- list_inc(?TYPE,lists:nth(?NOENTR,List),lists:nth(?TYPE,List),List,?TYPE)],
	 [lists:nth(S, List) || S <- list_inc(?SECID,lists:nth(?NOENTR,List),lists:nth(?TYPE,List),List,?TYPE)],%
	 [lists:nth(R, List) || R <- list_inc(?RPTSEQ,lists:nth(?NOENTR,List),lists:nth(?TYPE,List),List,?TYPE)],
	 [lists:nth(E, List) || E <- list_inc(?EXP,lists:nth(?NOENTR,List),lists:nth(?TYPE,List),List,?TYPE)],
	 [lists:nth(M, List) || M <- list_inc(?MANT,lists:nth(?NOENTR,List),lists:nth(?TYPE,List),List,?TYPE)]).

template_snap(List) ->
    zip6([lists:nth(A, List) || A <- list_snap(26,lists:nth(12,List),lists:nth(13,List),List,13)],
	 [lists:nth(T, List) || T <- list_snap(13,lists:nth(12,List),lists:nth(13,List),List,13)],
	 [lists:nth(S, List) || S <- list_sign(9,lists:nth(12,List))],%
	 [lists:nth(R, List) || R <- list_sign(6,lists:nth(12,List))],
	 [lists:nth(E, List) || E <- list_snap(?EXP,lists:nth(12,List),lists:nth(13,List),List,13)],
	 [lists:nth(M, List) || M <- list_snap(?MANT,lists:nth(12,List),lists:nth(13,List),List,13)]).

list_inc(F,1,_P,_L,_S) ->
    [F];
list_inc(F,N,P,L,S) when P == 50 orelse P == 55 orelse P == 57 orelse P == 66 ->
    [F|list_inc(F + ?STEPB, N - 1, lists:nth(S + ?STEPB, L), L, S + ?STEPB)];
list_inc(F,N,P,L,S) ->
    [F|list_inc(F + ?STEPS, N - 1, lists:nth(S + ?STEPS, L), L, S + ?STEPS)].

list_snap(F,1,_P,_L,_S) ->
    [F];
list_snap(F,N,P,L,S) when P == 50 orelse P == 52 orelse P == 55 orelse P == 57 orelse P == 66 ->
    [F|list_snap(F + 16, N - 1, lists:nth(S + 16, L), L, S + 16)];
list_snap(F,N,P,L,S) ->
    [F|list_snap(F + 15, N - 1, lists:nth(S + 15, L), L, S + 15)].

list_sign(_Sign, 0) ->
    [];
list_sign(Sign, 1) ->
    [Sign];
list_sign(Sign, N) ->
    [Sign|list_sign(Sign, N - 1)].

zip6(X, Y, S, R, E, M) ->
    zip6_acc([], X, Y, S, R, E, M).

zip6_acc(Acc, [], _, _, _, _, _) ->
    lists:reverse(Acc);
zip6_acc(Acc, [X|Xs], [Y|Ys], [S|Ss], [R|Rs], [E|Es], [M|Ms]) ->
    zip6_acc([{X, Y, S, R, E, M}|Acc], Xs, Ys, Ss, Rs, Es, Ms).

extract([]) ->
    ok;
extract([H|T]) when is_tuple(H) andalso T == [] ->
    fast_dispatcher:feed(H);
extract([H|T]) when is_tuple(H) ->
    %?D (H),
    fast_dispatcher:feed(H),
    extract(T).
