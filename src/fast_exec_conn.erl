-module(fast_exec_conn).
-behavior(gen_server).

-include("log.hrl").

-include("../include/admin.hrl").
-include("../include/business.hrl").
-include("../include/fix.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(NETWORK_TIMEOUT, 500).

-export([start_link/1, connect/2, new_order_single/6, cancel_order/7]).

-export([init/1,  handle_call/3, handle_info/2, terminate/2]).

-record(conn, {socket,
	       seq = 1,
	       host,
	       port,
	       transport,
	       password,
	       sender,
	       target,
	       heartbeat,
	       logon_from,
	       consumer,
	       buffer = <<>>,
	       send_to
}).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

connect(Pid, SendTo) ->
    gen_server:call(Pid, {connect_logon, SendTo}).

new_order_single(Pid, OrderId, Stock, Side, Quantaty, Options) ->
    NewOrder = [{transact_time, fast:now()}, {order_qty, Quantaty} | Options],
    ok = gen_server:call(Pid, {msg, new_order_single, OrderId, Side, Stock, NewOrder}),
    {ok, OrderId}.

cancel_order(Pid, OrderId, CancelId, Stock, Side, Quantaty, Options) ->
    OrderCancel = [{orig_cl_ord_id, CancelId}, {transact_time, fast:now()}, {order_qty, Quantaty} | Options],
    ok = gen_server:call(Pid, {msg, order_cancel_request, OrderId, Side, Stock, OrderCancel}),
    ok.

init([Name]) ->
    fast:start_task(fast_m, 2, {fast_fsm, start_link, []}),
    fast:run(fast_m, [self()]),
    {ok, Options} = application:get_env(fast, Name),
    {host, Host} = lists:keyfind(host, 1, Options),
    {port, Port} = lists:keyfind(port, 1, Options),
    {password, Password} = lists:keyfind(password, 1, Options),
    {sender, Sender} = lists:keyfind(sender, 1, Options),
    {target, Target} = lists:keyfind(target, 1, Options),
    Heartbeat = proplists:get_value(heartbeat, Options, 30),
    {ok, #conn{host = Host,
	       port = Port,
	       password = Password,
	       sender = Sender,
	       target = Target,
	       heartbeat = Heartbeat
	      }}.

handle_call({connect_logon, SendTo}, From, #conn{} = Conn) ->
    Connected = #conn{} = do_connect(Conn),
    Logon_sent = send_logon(Connected#conn{send_to = SendTo, logon_from = From}),
    {noreply, Logon_sent, ?NETWORK_TIMEOUT};
handle_call({msg, MessageType, ClOrdId, Side, Stock, Tail}, {_Owner, _Ref}, #conn{} = Conn) ->
    Body = fast:stock_to_instrument_block(Stock) ++ [{side, Side}, {cl_ord_id, ClOrdId}|Tail],
 %   NewConn = remember_request(MessageType, ClOrdId, Owner, Conn},
    {reply, ok, send(MessageType, Body, Conn)}.

handle_info(heartbeat, #conn{} = Conn) ->
    {noreply, send(heartbeat, [], Conn)};
handle_info({tcp, Socket, Bin}, #conn{buffer = PreBuffer} = Conn) ->
    Buffer = <<PreBuffer/binary, Bin/binary>>,
    {Messages, NewConn} = case decode(Buffer) of
			      {Msgs, Rest} ->
				  {Msgs, Conn#conn{buffer = Rest}};
			      {Msgs, Rest, _} ->
				  {Msgs, Conn#conn{buffer = Rest}}
			  end,
    inet:setopts(Socket, [{active, once}]),
    handle_messages(Messages, NewConn);
handle_info({Closed, _Socket}, #conn{} = Conn) when Closed == tcp_closed ->
    {stop, {shutdown, socket_closed}, Conn};
handle_info({'EXIT', _From, Reason}, #conn{} = Conn) ->
    {stop, Reason, Conn}.

do_connect(#conn{host = Host, port = Port} = Conn) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, raw}, {send_timeout,?NETWORK_TIMEOUT}, {active, once}]),
    Conn#conn{socket = Socket}.

send_logon(#conn{password = Password, heartbeat = Heartbeat} = Conn) ->
    MsgBody = [{encrypt_method, 0},{heart_bt_int, Heartbeat},{reset_seq_num_flag, "Y"},{password, Password}] ++ [{10001,"Y"}],

    timer:send_interval(Heartbeat*1000, heartbeat),
    send(logon, MsgBody, Conn).

send(MessageType, Body, #conn{seq = Seq, sender = Sender, target = Target, socket = Socket} = Conn) ->
    Bin = fast:pack(MessageType, Body, Seq, Sender, Target),
    Result = gen_tcp:send(Socket, Bin),
    ok = Result,
    Conn#conn{seq = Seq + 1}.

terminate(_, _) ->
    ok.

decode(Bin) ->
    decode(Bin, []).

decode(Bin, Acc) ->
    case fast:decode_bin(Bin) of
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

handle_messages([{#heartbeat{},_}|Messages], #conn{} = Conn) ->
    handle_messages(Messages, Conn);
handle_messages([{#logon{},_}|Messages], #conn{logon_from = {Pid, _} = From} = Conn) ->
    gen_server:reply(From, ok),
    erlang:monitor(process, Pid),
    handle_messages(Messages, Conn#conn{logon_from = undefined, consumer = Pid});
handle_messages([{#execution_report{ord_status = Status, leaves_qty = LQ, cum_qty = CQ, side = Side}, _}|Messages],
		#conn{send_to = SendTo} = Conn) ->
    ?D({execution_report, Status, LQ, CQ, Side}),
    case Status of
	partial ->
	    fast_fsm:partial(SendTo, CQ),
	    fast_fsm:orders_book({Status, CQ, Side});
	filled ->
	    fast_fsm:filled(SendTo, CQ),
	    fast_fsm:orders_book({Status, CQ, Side});
	canceled ->
	    fast_fsm:canceled(SendTo);
	rejected ->
	    fast_fsm:orders_book({Status, CQ, Side});
	_ ->
	    ok
    end,
    handle_messages(Messages, Conn);
handle_messages([{Unknown, Bin}|Messages], #conn{consumer = Consumer} = Conn) when Consumer /= undefined ->
    Consumer ! #fix{pid = self(), message = Unknown, bin = Bin},
    handle_messages(Messages, Conn);
handle_messages([], #conn{} = Conn) ->
    {noreply, Conn}.