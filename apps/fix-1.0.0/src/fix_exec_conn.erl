-module(fix_exec_conn).
-behavior(gen_server).

-include("log.hrl").

-include("../include/admin.hrl").
-include("../include/business.hrl").
-include("../include/fix.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(NETWORK_TIMEOUT, 500).

-export([start_link/2, connect/1, new_order_single/5, cancel_order/6, close/1]).

-export([init/1,  handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(conn, {socket,
%	       seq = 1,
	       table_id,
	       host,
	       port,
	       transport,
	       password,
	       sender,
	       target,
	       heartbeat,
	       account,
	       logon_from,
	       consumer,
	       buffer = <<>>,
	       send_to,
	       futures,
	       tref,
	       check_seq_num,
	       check,
	       heartbeatnum = 0,
	       seq_test_req = 0,
	       fd
	       
}).

start_link(Name, Instrument) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Name, Instrument], []).

connect(Pid) ->
    gen_server:call(Pid, connect_logon).

new_order_single(Pid, OrderId, Side, Quantaty, Options) ->
    NewOrder = [{transact_time, fix:now()}, {order_qty, Quantaty} | Options],
    ok = gen_server:call(Pid, {msg, new_order_single, OrderId, Side, NewOrder}),
    {ok, OrderId}.

cancel_order(Pid, OrderId, CancelId, Side, Quantaty, Options) ->
    OrderCancel = [{orig_cl_ord_id, CancelId}, {transact_time, fix:now()}, {order_qty, Quantaty} | Options],
    ok = gen_server:call(Pid, {msg, order_cancel_request, OrderId, Side, OrderCancel}),
    ok.

close(Pid) ->
    gen_server:call(Pid, logout).

init([Name, Instrument]) ->
%    gen_server:cast(self(), connect_logon),
    ?D(self()),
    {ok, FD} = file:open("/tmp/trades.log", [write, append]),
    {ok, Options} = application:get_env(fix, Name),
    {host, Host} = lists:keyfind(host, 1, Options),
    {port, Port} = lists:keyfind(port, 1, Options),
    {password, Password} = lists:keyfind(password, 1, Options),
    {sender, Sender} = lists:keyfind(sender, 1, Options),
    {target, Target} = lists:keyfind(target, 1, Options),
    Heartbeat = proplists:get_value(heartbeat, Options, 30),
    {account, Account} = lists:keyfind(account, 1, Options),
    {ok, #conn{host = Host,
	       port = Port,
	       password = Password,
	       sender = Sender,
	       target = Target,
	       heartbeat = Heartbeat,
	       account = Account,
	       futures = Instrument,
	       fd = FD
	      }}.

handle_call(connect_logon, _From, #conn{} = Conn) ->
    %?D (From),
    Connected = #conn{} = do_connect(Conn),
    Logon_sent = send_logon(Connected),
    {noreply, Logon_sent, ?NETWORK_TIMEOUT};
handle_call(logout, _From, #conn{} = Conn) ->
    send_logout(Conn),
    {reply, ok, Conn#conn{socket = closed}};
handle_call({msg, MessageType, ClOrdId, Side, Tail}, {_Owner, _Ref}, #conn{account = Account, futures = Futures} = Conn) ->
    Body = fix:stock_to_instrument_block(Futures) ++ [{side, Side}, {cl_ord_id, ClOrdId}, {account, atom_to_binary(Account, latin1)}|Tail],
    {reply, ok, send(MessageType, Body, Conn)}.

%handle_cast(reconnect_logon, #conn{} = Conn) ->
    %?D (From),
%    Connected = #conn{} = do_connect(Conn),
%    Logon_sent = send_logon(Connected),
%    {noreply, Logon_sent};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(heartbeat, #conn{} = Conn) ->
   {noreply, send(heartbeat, [], Conn)};
handle_info({check, Check_Num}, #conn{heartbeatnum = Heartbeat, seq_test_req = Seq_Test_Req, socket = Socket, send_to = SendTo} = Conn) ->
    if Check_Num =< Heartbeat ->
	    ?D({Check_Num, Heartbeat}),
	    {noreply, Conn};
	true ->
	    case Seq_Test_Req of
		0 ->
		    ?D(Check_Num),
		    erlang:send_after(31000, self(), {check, Check_Num}),
		    {noreply, send(test_request, [], Conn#conn{seq_test_req = 1})};
		1 ->
		    ars_fsm:break_conn(SendTo),
		    gen_tcp:close(Socket),
		    erlang:send_after(100, self(), reconnect),
		    {noreply, Conn#conn{socket = undefined, seq_test_req = 0}}
	    end
    end;
    %erlang:cancel_timer(Check),
    %{noreply, Conn};
handle_info({tcp, Socket, Bin}, #conn{buffer = PreBuffer} = Conn) ->
    Buffer = <<PreBuffer/binary, Bin/binary>>,
    {Messages, NewConn} = case decode(Buffer) of
			      {Msgs, Rest} ->
				  {Msgs, Conn#conn{buffer = Rest}};
			      {Msgs, Rest, _} ->
				  {Msgs, Conn#conn{buffer = Rest}}
			  end,
    inet:setopts(Socket, [{active, once}]),
    ?D(Messages),
    handle_messages(Messages, NewConn);
handle_info(reconnect, #conn{} = Conn) ->
    gen_server:call(self(), connect_logon),
    {noreply, Conn};
handle_info({Closed, _Socket}, #conn{tref = TRef} = Conn) when Closed == tcp_closed ->
    timer:cancel(TRef),
    {noreply, Conn#conn{socket = closed, tref = canceled}};
handle_info({'ETS-TRANSFER', TableId, Pid, _Data}, Conn) ->
    io:format("MGR(~p) -> FEC(~p) getting TableId:~p~n", [Pid, self(), TableId]),
    {noreply, Conn#conn{table_id = TableId}};
handle_info({'EXIT', _From, Reason}, #conn{} = Conn) ->
    {stop, Reason, Conn};
handle_info(Msg, #conn{} = Conn) ->
    ?D(Msg),
    {stop, Conn}.

do_connect(#conn{host = Host, port = Port} = Conn) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, raw}, {send_timeout,?NETWORK_TIMEOUT}, {active, once}]),
    Conn#conn{socket = Socket}.

send_logon(#conn{password = Password, heartbeat = Heartbeat} = Conn) ->
    MsgBody = [{encrypt_method, 0},{heart_bt_int, Heartbeat},{reset_seq_num_flag, "N"},{password, Password}] ++ [{10001,"Y"}],
    {ok, TRef} = timer:send_interval(Heartbeat*1000, heartbeat),
    send(logon, MsgBody, Conn#conn{tref = TRef}).

send_logout(#conn{password = Password} = Conn) ->
    MsgBody = [{password, Password}], %++ [{10001,"Y"}],
    send(logout, MsgBody, Conn).

%send_resend_request(#conn{} = Conn) ->
%    MsgBody = [{begin_seq_no, }, {end_seq_no, 0}],
%    send(resend_request, MsgBody, Conn).

send({MessageType, _Option}, Body, #conn{} = Conn) ->
    send(MessageType, Body, Conn);
send(MessageType, Body, #conn{table_id = TableId, sender = Sender, target = Target, socket = Socket} = Conn) ->
    [{_, Seq}] = ets:lookup(TableId, seq),
    ?D(Seq),
    case Socket of
	closed ->
	    io:format("Socket_closed. ~n");
	_ ->
	    Bin = fix:pack(MessageType, Body, Seq, Sender, Target),
	    Result = gen_tcp:send(Socket, Bin),
	    ok = Result,
	    NewSeq = Seq + 1,
	    ets:insert(TableId, {seq, NewSeq}),
	    Conn
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Msg, #conn{check = Check, fd = FD}) ->
    erlang:cancel_timer(Check),
    file:close(FD),
    ok.

decode(Bin) ->
    decode(Bin, []).

decode(Bin, Acc) ->
    case fix:decode_bin(Bin) of
	{ok, Msgs, MsgBin, Rest} ->
	    decode(Rest,[{Msgs, MsgBin}|Acc]);
	{more, _} ->
	    return_decoded(lists:reverse(Acc), Bin);
	error ->
	    ?D({error, Bin}),
	    erlang:error(broken_fix)
    end.

return_decoded(Msgs, Rest) ->
	{Msgs, Rest}.

handle_messages([{#heartbeat{msg_seq_num = Num},_}|Messages], #conn{table_id = TableId} = Conn) ->
%    ?D(Num),
    ets:insert(TableId, {prev, Num}),
    Check_Num = Num + 1,
    Check = erlang:send_after(31000, self(), {check, Check_Num}),
    handle_messages(Messages, Conn#conn{heartbeatnum = Num, check = Check});
handle_messages([{#logon{msg_seq_num = Num},_}|Messages], #conn{table_id = TableId} = Conn) ->
    Prev = case ets:lookup(TableId, prev) of
	       [{ _, Prev_Num}] ->
		   Prev_Num + 1;
	       _ ->
		   1
	   end,
    case Num of
	Prev ->
	    ok;
	_ ->
	    MsgBody = [{begin_seq_no, Prev}, {end_seq_no, Num}],
	    send(resend_request, MsgBody, Conn)
    end,
    ets:insert(TableId, {prev, Num}),
    SendTo = whereis(ars_fsm),
    ars_fsm:conn(SendTo),
    Check_Num = Num + 1,
    Check = erlang:send_after(31000, self(), {check, Check_Num}),
    handle_messages(Messages, Conn#conn{heartbeatnum = Num, check = Check, send_to = SendTo});
handle_messages([{#resend_request{msg_seq_num = Num},_}|Messages], #conn{table_id = TableId} = Conn) ->
    ets:insert(TableId, {prev, Num}),
    Check_Num = Num + 1,
    Check = erlang:send_after(31000, self(), {check, Check_Num}),
    handle_messages(Messages, Conn#conn{heartbeatnum = Num, check = Check});
handle_messages([{#logout{text = Text}, _Bin}|Messages], #conn{logon_from = {_, _} = From} = Conn) ->
    ?D(Text),
    gen_server:reply(From, ok),
    handle_messages(Messages, Conn);
handle_messages([{#execution_report{msg_seq_num = Num, ord_status = Status, cum_qty = CQ, side = Side, price = Price}, _Bin}|Messages],
		#conn{send_to = SendTo, table_id = TableId, fd = FD} = Conn) ->
    ets:insert(TableId, {prev, Num}),
    Check_Num = Num + 1,
    Check = erlang:send_after(31000, self(), {check, Check_Num}),
    
    ?D({execution_report, Status, CQ, Side, SendTo}),
%	?D(fix:dump(Bin)),
    case Status of
	partial ->
	    ars_fsm:partial(SendTo, CQ, Side),
	    file:write(FD,io_lib:format("Status: ~p Side: ~p Quant: ~p Price: ~p\~n", [Status, Side, CQ, Price]));
	filled ->
	    ars_fsm:filled(SendTo, CQ, Side);
%	    ars_fsm:orders_book({Status, LQ, CQ, Side});
	canceled ->
	    ars_fsm:canceled(SendTo);
%	    ars_fsm:orders_book({Status, LQ, CQ, Side});
	rejected ->
	    ars_fsm:rejected(SendTo);
%	    ars_fsm:orders_book({Status, LQ, CQ, Side});
	expired ->
	    ars_fsm:expired(SendTo);
	_ ->
	    io:format("Status: ~p~n", [Status])
    end,
    handle_messages(Messages, Conn#conn{heartbeatnum = Num, check = Check});
handle_messages([{#order_cancel_reject{msg_seq_num = Num, cxl_rej_reason = Cxl_Rej_Reason}, _Bin}|Messages],
		#conn{send_to = SendTo, table_id = TableId} = Conn) ->
    ets:insert(TableId, {prev, Num}),
    Check_Num = Num + 1,
    Check = erlang:send_after(31000, self(), {check, Check_Num}),
%    ?D(Cxl_Rej_Reason),
    case Cxl_Rej_Reason of
	0 ->
	    ars_fsm:executed(SendTo),
	    ?D(Cxl_Rej_Reason);
	1 ->
	    ars_fsm:unknown(SendTo),
	    ?D(Cxl_Rej_Reason);
	3 ->
	    ars_fsm:replaced(SendTo),
	    ?D(Cxl_Rej_Reason);
	_ ->
	    ars_fsm:other(SendTo),
	    ?D(Cxl_Rej_Reason)
    end,
    handle_messages(Messages, Conn#conn{heartbeatnum = Num, check = Check});
handle_messages([{#sequence_reset{msg_seq_num = Num}, _Bin}|Messages], #conn{table_id = TableId} = Conn) ->
    ets:insert(TableId, {prev, Num}),
    Check_Num = Num + 1,
    Check = erlang:send_after(31000, self(), {check, Check_Num}),
    handle_messages(Messages, Conn#conn{heartbeatnum = Num, check = Check});
handle_messages([{Unknown, Bin}|Messages], #conn{consumer = Consumer} = Conn) when Consumer /= undefined ->
    Consumer ! #fix{pid = self(), message = Unknown, bin = Bin},
    io:format("Unknown Messages: ~p~n", [Unknown]),
    handle_messages(Messages, Conn);
handle_messages([], #conn{} = Conn) ->
    {noreply, Conn}.
