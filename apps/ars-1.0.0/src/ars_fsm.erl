-module(ars_fsm).
-behaviour(gen_fsm).

-include("log.hrl").

-define(LIMIT, 3).

-record(state, {
	  pid_exec_conn,
	  pid_event,
	  count = 1,
	  ord_qty = 0,
	  partial_qty = 0,
	  price = 0,
	  order_count = 0,
	  side,
	  order_state,
	  state_name,
	  table_id,
	  cncl_ord_side,
	  cncl_qty
}).

%% public API
-export([start_link/0, terminate/3, code_change/4, handle_info/3, handle_event/3, handle_sync_event/4]).

%% callbacks
-export([init/1]).

%% custom state names
-export([idle/2, wait/2, buying2long/2, selling2short/2, sell_market/2, buy_market/2, buy_cash/2, sell_cash/2,
	 canceled_to_market_selling/2, partial_short/2,
	 partial_long/2, canceled_to_market_buying/2, long/2, short/2, ready_to_sell/1, ready_to_buy/1, orders_book/1, 
	 canceled_buying/2, canceled_selling/2, selling2cash/2, buying2cash/2]).

% events
-export([buy/2, sell/2, tp_buy/2, tp_sell/2, repeat_up/2, repeat_down/2, rejected/1, expired/1, close/1, closing/2,  executed/1, unknown/1,
	 break_conn/1, conn/1, replaced/1, other/1, ready_sell/2, ready_buy/2, market_sell/2, market_buy/2, filled/3, partial/3, canceled/3]).
% privat
%-export([connect/3]).

start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

buy(Pid, Number) ->
    gen_fsm:send_event(Pid, {buy, Number}).

sell(Pid, Number) ->
    gen_fsm:send_event(Pid, {sell, Number}).

tp_buy(Pid, Number) ->
    gen_fsm:send_event(Pid, {tp_buy, Number}).

tp_sell(Pid, Number) ->
    gen_fsm:send_event(Pid, {tp_sell, Number}).

ready_to_sell(Pid) ->
    gen_fsm:send_event(Pid, ready_to_sell).

market_sell(Pid,  Number) ->
    gen_fsm:send_event(Pid, {market_sell, Number}).

ready_to_buy(Pid) ->
    gen_fsm:send_event(Pid, ready_to_buy).

market_buy(Pid,  Number) ->
    gen_fsm:send_event(Pid, {market_buy, Number}).

repeat_up(Pid, Number) ->
    gen_fsm:send_event(Pid, {move_up, Number}).

repeat_down(Pid, Number) ->
    gen_fsm:send_event(Pid, {move_down, Number}).

partial(Pid, Qty, Side) ->
    gen_fsm:send_event(Pid, {partial, Qty, Side}).

filled(Pid, Qty, Side) ->
    gen_fsm:send_event(Pid, {filled, Qty, Side}).

canceled(Pid, Qty, Side) ->
    gen_fsm:send_event(Pid, {canceled, Qty, Side}).

rejected(Pid) ->
    gen_fsm:send_event(Pid, rejected).

expired(Pid) ->
    gen_fsm:send_event(Pid, expired).

executed(Pid) ->
    gen_fsm:send_event(Pid, executed).

unknown(Pid) ->
    gen_fsm:send_event(Pid, unknown).

replaced(Pid) ->
    gen_fsm:send_event(Pid, replaced).

other(Pid) ->
    gen_fsm:send_event(Pid, other).

conn(Pid) ->
    gen_fsm:send_event(Pid, conn).

break_conn(Pid) ->
    gen_fsm:send_event(Pid, break_conn).

close(Pid) ->
    gen_fsm:send_all_state_event(Pid, close).

init([]) ->
    ars:start_service(handler, {ars_handler, start_link, []}),
    ars:run_task(handler, [self()]),
    {ok, PidTo} = gen_event:start_link(),
    gen_event:add_handler(PidTo, ars_msg_sender, []),
%    ets:new(orders, [named_table, set, public]),
    {ok, idle, #state{pid_event = PidTo}}.

idle(conn, #state{state_name = State_Name} = S) ->
    case State_Name of
	undefined ->
	    {next_state, wait, S};
	_ ->
	    {next_sate, State_Name, S}
    end;
idle(_Event, S) ->
    {next_state, idle, S}.

wait({buy, Price}, #state{pid_event=PidTo, count=Count} = S) ->
    SendTo = whereis(fix_exec_conn),
    OrderQty = ?LIMIT,
    ?D({buy_at, Price, OrderQty}),
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, buy, OrderQty, [{ord_type,2},{price,Price}]}),
    {next_state, buying2long,
     S#state{pid_exec_conn=SendTo, count=Count+1, order_count=Count, ord_qty=OrderQty, price=Price, side='buy', order_state = placed}};
wait({sell, Price}, #state{pid_event=PidTo, count=Count} = S) ->
    SendTo = whereis(fix_exec_conn),
    OrderQty = ?LIMIT,
    ?D({sell_at, Price, OrderQty}),
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, sell, OrderQty, [{ord_type,2},{price, Price}]}),
    {next_state, selling2short,
     S#state{pid_exec_conn=SendTo, count=Count+1, order_count=Count, ord_qty=OrderQty, price=Price, side='sell', order_state = placed}};
wait(break_conn, #state{} = S) ->
    {next_state, idle, S#state{state_name = wait}};
wait(_E, S) ->
    {next_state, wait, S}.

buying2long(rejected, #state{} = S) ->
    ?D(rejected),
    {next_state, buying2long, S#state{order_state = rejected}};
buying2long(expired, #state{} = S) ->
    ?D(expired),
    {next_state, buying2long, S#state{order_state = expired}};
buying2long({filled, Qty, Side}, #state{table_id = TableId} = S) ->
    orders_book({filled, Qty, Side, TableId}),
    ?D({filled, Qty, Side, TableId}),
    {next_state, long, S#state{order_state = filled}};
buying2long({partial, Qty, Side}, #state{table_id = TableId} = S) ->
    orders_book({partial, Qty, Side, TableId}),
    ?D({partial, Qty, Side, TableId}),
    {next_state, buying2long, S#state{order_state = partial, partial_qty = Qty}};
buying2long({move_up, Price},
       #state{pid_exec_conn=SendTo, pid_event=PidTo, count=Count, order_count=Order_Count, ord_qty=OrderQty, side = Side} = S) ->
    ?D({move_buy_at, Price}),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, Side, OrderQty, [{ord_type,2}]}),
    {next_state, canceled_buying, S#state{count = Count + 1, price = Price, 
					  order_state = canceled, cncl_ord_side = Side, cncl_qty = OrderQty}};
buying2long({sell, Price},
       #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, order_count = Order_Count, ord_qty = OrderQty, side = Side} = S) ->
    ?D({reverse_sell_at, Price, from_buying2long}),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, Side, OrderQty, [{ord_type,2}]}),
    {next_state, canceled_selling, S#state{count = Count + 1, price = Price, 
					   side = 'sell', order_state = canceled, cncl_ord_side = Side, cncl_qty = OrderQty}};
buying2long(ready_to_sell,
       #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, order_count = Order_Count, ord_qty = OrderQty, side = Side} = S) ->
    ?D(ready_to_sell_from_buying2long),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, Side, OrderQty, [{ord_type,2}]}),
    {next_state, ready_sell, S#state{count = Count + 1, order_state = canceled, cncl_ord_side = Side, cncl_qty = OrderQty}};

buying2long(break_conn, #state{} = S) ->
    {next_state, idle, S#state{state_name = buying2long}};
buying2long(_E, S) ->
    {next_state, buying2long, S}.

selling2short(rejected, #state{} = S) ->
    ?D(rejected),
    {next_state, selling2short, S#state{order_state = rejected}};
selling2short(expired, #state{} = S) ->
    ?D(expired),
    {next_state, selling2short, S#state{order_state = expired}};
selling2short({filled, Qty, Side}, #state{table_id = TableId} = S) ->
    orders_book({filled, Qty, Side, TableId}),
    ?D({filled, Qty, Side}),
    {next_state, short, S#state{order_state = filled}};
selling2short({partial, Qty, Side}, #state{table_id = TableId} = S) ->
    orders_book({partial, Qty, Side, TableId}),
    ?D({partial, Qty, TableId}),
    {next_state, selling2short, S#state{order_state = partial, partial_qty = Qty}};
selling2short({move_down, Price},
	#state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, order_count = Order_Count, ord_qty = OrderQty, side = Side} = S) ->
    ?D({move_sell_at, Price}),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, Side, OrderQty, [{ord_type,2}]}),
    {next_state, canceled_selling, S#state{count = Count + 1, price = Price, 
					   order_state = canceled, cncl_ord_side = Side, cncl_qty = OrderQty}};
selling2short({buy, Price},
	#state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, order_count = Order_Count, ord_qty = OrderQty, side = Side} = S) ->
    ?D({buy_at, Price, from_selling2short}),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, Side, OrderQty, [{ord_type,2}]}),
    {next_state, canceled_buying, S#state{count = Count + 1, price = Price, 
					  side = 'buy', order_state = canceled, cncl_ord_side = Side, cncl_qty = OrderQty}};
selling2short(ready_to_buy,
	#state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, order_count = Order_Count, ord_qty = OrderQty, side = Side} = S) ->
    ?D(ready_to_buy_from_selling2short),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, Side, OrderQty, [{ord_type,2}]}),
    {next_state, ready_buy, S#state{count = Count + 1, cncl_ord_side = Side, cncl_qty = OrderQty}};

selling2short(break_conn, #state{} = S) ->
    {next_state, idle, S#state{state_name = selling2short}};
selling2short(_E, S) ->
    {next_state, selling2short, S}.

canceled_buying(Status, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count,
				 price = Price, side = Side, table_id = TableId, cncl_ord_side = Cncl_Side, cncl_qty = Cncl_Qty} = S) ->
    ?D(canceled_buying_limit),
    Pos = position(TableId),
%    ?D({new_order, Pos, Side, at, Price}),
    if Pos < 0 ->
	    case Status of
		{canceled, Qty, Ord_Side} ->
		    Ord_Qty = -Pos + ?LIMIT,
		    gen_event:notify(PidTo, {new_order_single, SendTo, Count, Side, Ord_Qty, [{ord_type, 2}, {price, Price}]}),
		    orders_book({canceled, Qty, Ord_Side, TableId}),
		    {next_state, buying2long, S#state{count = Count + 1, order_count = Count, ord_qty = Ord_Qty, order_state = placed}};
		_ ->
		    Ord_Qty = Pos + ?LIMIT,
		    gen_event:notify(PidTo, {new_order_single, SendTo, Count, Side, Ord_Qty, [{ord_type, 2}, {price, Price}]}),
		    orders_book({canceled, Cncl_Qty, Cncl_Side, TableId}),
		    {next_state, buying2long, S#state{count = Count + 1, order_count = Count, ord_qty = Ord_Qty, order_state = placed}}
	    end;
        Pos >= 0 andalso Pos < ?LIMIT ->
	    case Status of
		{canceled, Qty, Ord_Side} ->
		    Ord_Qty = ?LIMIT - Pos,
		    gen_event:notify(PidTo, {new_order_single,SendTo,Count, Side, Ord_Qty, [{ord_type,2},{price, Price}]}),
		    orders_book({canceled, Qty, Ord_Side, TableId}),
		    {next_state, buying2long, S#state{count = Count + 1, order_count = Count, ord_qty = Ord_Qty, order_state = placed}};
		_ ->
		    Ord_Qty = ?LIMIT - Pos,
		    gen_event:notify(PidTo, {new_order_single,SendTo,Count, Side, Ord_Qty, [{ord_type,2},{price, Price}]}),
		    orders_book({canceled, Cncl_Qty, Cncl_Side, TableId}),
		    {next_state, buying2long, S#state{count = Count + 1, order_count = Count, ord_qty = Ord_Qty, order_state = placed}}
	    end;
       Pos >= ?LIMIT ->
	    case Status of
		{canceled, Qty, Ord_Side} ->
		    orders_book({canceled, Qty, Ord_Side, TableId}),
		    {next_state, long, S};
		_ ->
		    orders_book({canceled, Cncl_Qty, Cncl_Side, TableId}),
		    {next_state, long, S}
	    end
    end;
canceled_buying(break_conn, #state{} = S) ->
    {next_state, idle, S#state{state_name = canceled_buying}};
canceled_buying(_E, S) ->
    {next_state, canceled_buying, S}.

canceled_selling(Status, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count,
				  price = Price, side = Side, table_id = TableId, cncl_ord_side = Cncl_Side, cncl_qty = Cncl_Qty} = S) ->
    ?D(canceled_selling_limit),
    Pos = position(TableId),
    ?D({new_order, Pos, Side, at, Price}),
    if Pos > 0 ->
	    case Status of
		{canceled, Qty, Ord_Side} ->
		    Ord_Qty = Pos + ?LIMIT,
		    gen_event:notify(PidTo,{new_order_single, SendTo, Count, Side, Ord_Qty, [{ord_type,2},{price, Price}]}),
		    orders_book({canceled, Qty, Ord_Side, TableId}),
		    {next_state, selling2short, S#state{count = Count + 1, order_count = Count, ord_qty = Ord_Qty, order_state = placed}};
		_ ->
		    Ord_Qty = Pos + ?LIMIT,
		    gen_event:notify(PidTo,{new_order_single, SendTo, Count, Side, Ord_Qty, [{ord_type,2},{price, Price}]}),
		    orders_book({canceled, Cncl_Qty, Cncl_Side, TableId}),
		    {next_state, selling2short, S#state{count = Count + 1, order_count = Count, ord_qty = Ord_Qty, order_state = placed}}
	    end;
       Pos =< 0 andalso Pos > -?LIMIT ->
	    case Status of
		{canceled, Qty, Ord_Side} ->
		    Ord_Qty = ?LIMIT + Pos,
		    gen_event:notify(PidTo,{new_order_single, SendTo, Count, Side, Ord_Qty, [{ord_type,2},{price, Price}]}),
		    orders_book({canceled, Qty, Ord_Side, TableId}),
		    {next_state, selling2short, S#state{count = Count + 1, order_count = Count, ord_qty = Ord_Qty, order_state = placed}};
		_ ->
		    Ord_Qty = ?LIMIT + Pos,
		    gen_event:notify(PidTo,{new_order_single, SendTo, Count, Side, Ord_Qty, [{ord_type,2},{price, Price}]}),
		    orders_book({canceled, Cncl_Qty, Cncl_Side, TableId}),
		    {next_state, selling2short, S#state{count = Count + 1, order_count = Count, ord_qty = Ord_Qty, order_state = placed}}
	    end;
       Pos =< -?LIMIT ->
	    case Status of
		{canceled, Qty, Ord_Side} ->
		    orders_book({canceled, Qty, Ord_Side, TableId}),
		    {next_state, short, S};
		 _ ->
		    orders_book({canceled, Cncl_Qty, Cncl_Side, TableId}),
		    {next_state, short, S}
	    end
    end;
canceled_selling(break_conn, #state{} = S) ->
    {next_state, idle, S#state{state_name = canceled_selling}};
canceled_selling(_E, State) ->
    {next_state, canceled_selling, State}.

long({sell, Price}, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count} = S) ->
    ?D({sell_at, Price}),
    OrderQty = 2*?LIMIT,
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, sell, OrderQty, [{ord_type,2},{price, Price}]}),
    {next_state, selling2short,
     S#state{count = Count + 1, order_count = Count, ord_qty = OrderQty, price = Price, side = 'sell', order_state = placed}};
long(ready_to_sell, S) ->
    ?D(ready_to_sell),
    {next_state, ready_sell, S};
long({market_sell, Price}, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count} = S) ->
    ?D({sell_market_at, Price}),
    OrderQty = ?LIMIT,
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, sell, OrderQty, [{ord_type,2},{price, Price}]}),
    {next_state, sell_market,
     S#state{count = Count + 1, order_count = Count, ord_qty = OrderQty, price = Price, side = 'sell', order_state = placed}};
long(break_conn, #state{} = S) ->
    {next_state, idle, S#state{state_name = long}};
long(_E, S) ->
    {next_state, long, S}.

short({buy, Price}, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count} = S) ->
    ?D({buy_at, Price}),
    OrderQty = 2*?LIMIT,
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, buy, OrderQty, [{ord_type,2},{price, Price}]}),
    {next_state, buying2long,
     S#state{count = Count + 1, order_count = Count, ord_qty = OrderQty, price = Price, side = 'buy', order_state = placed}};
short(ready_to_buy, S) ->
    ?D(ready_to_buy),
    {next_state, ready_buy, S};
short({market_buy, Price}, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count} = S) ->
    ?D({buy_market_at, Price}),
    OrderQty = ?LIMIT,
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, buy, OrderQty, [{ord_type,2},{price, Price}]}),
    {next_state, buy_market,
     S#state{count = Count + 1, order_count = Count, ord_qty = OrderQty, price = Price, side = 'buy', order_state = placed}};
short(break_conn, #state{} = S) ->
    {next_state, idle, S#state{state_name = short}};
short(_E, S) ->
    {next_state, short, S}.

ready_sell({tp_sell, Price}, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, table_id = TableId} = S) ->
    Pos = position(TableId),
    ?D({tp_sell, Pos, at, Price}),
    if Pos > 0 ->
	    Ord_Qty = Pos,
	    gen_event:notify(PidTo,{new_order_single, SendTo, Count, sell, Ord_Qty,[{ord_type,2},{price, Price}]}),
	    {next_state, selling2cash,
	     S#state{count = Count + 1, order_count = Count, ord_qty = Ord_Qty, price = Price, side = 'sell', order_state = placed}};
       Pos == -?LIMIT ->
	    {next_state, short, S};
       Pos == 0 ->
	    {next_state, sell_cash, S};
       true ->
	    Ord_Qty = ?LIMIT - Pos,
	    {next_state, partial_short, S#state{ord_qty = Ord_Qty}}
    end;
ready_sell({market_sell, Price}, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, table_id = TableId} = S) ->
    Pos = position(TableId),
    ?D({market_sell, Pos, at, Price}),
    if Pos > 0 ->
	    Ord_Qty = Pos,
	    gen_event:notify(PidTo,{new_order_single, SendTo, Count, sell, Ord_Qty,[{ord_type,2},{price, Price}]}),
	    {next_state, selling2cash,
	     S#state{count = Count + 1, order_count = Count, ord_qty = Ord_Qty, price = Price, side = 'sell', order_state = placed}};
       Pos == -?LIMIT ->
	    {next_state, short, S};
       Pos == 0 ->
	    {next_state, sell_cash, S};
       true ->
	    Ord_Qty = ?LIMIT - Pos,
	    {next_state, partial_short, S#state{ord_qty = Ord_Qty}}
    end;
ready_sell(break_conn, #state{} = S) ->
    {next_state, idle, S#state{state_name = ready_sell}};
ready_sell(_E, S) ->
    {next_state, ready_sell, S}.

ready_buy({tp_buy, Price}, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, table_id = TableId} = S) ->
    Pos = position(TableId),
    ?D({tp_buy, Pos, at, Price}),
    if Pos < 0 ->
	    Ord_Qty = -1 * Pos,
	    gen_event:notify(PidTo,{new_order_single, SendTo, Count, buy, Ord_Qty,[{ord_type,2},{price, Price}]}),
	    {next_state, buying2cash,
	     S#state{count = Count + 1, order_count = Count, ord_qty = Ord_Qty, price = Price, side = 'buy', order_state = placed}};
       Pos == ?LIMIT ->
	    {next_state, long, S};
       Pos == 0 ->
	    {next_state, buy_cash, S};
       true ->
	    Ord_Qty = ?LIMIT - Pos,
	    {next_state, partial_long, S#state{ord_qty = Ord_Qty}}
    end;
ready_buy({market_buy, Price}, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, table_id = TableId} = S) ->
    Pos = position(TableId),
    ?D({market_buy, Pos, at, Price}),
    if Pos < 0 ->
	    Ord_Qty = -1 * Pos,
	    gen_event:notify(PidTo,{new_order_single, SendTo, Count, buy, Ord_Qty,[{ord_type,2},{price, Price}]}),
	    {next_state, buying2cash,
	     S#state{count = Count + 1, order_count = Count, ord_qty = Ord_Qty, price = Price, side = 'buy', order_state = placed}};
       Pos == ?LIMIT ->
	    {next_state, long, S};
       Pos == 0 ->
	    {next_state, buy_cash, S};
       true ->
	    Ord_Qty = ?LIMIT - Pos,
	    {next_state, partial_long, S#state{ord_qty = Ord_Qty}}
    end;
ready_buy(break_conn, #state{} = S) ->
    {next_state, idle, S#state{state_name = ready_buy}};
ready_buy(_E, S) ->
    {next_state, ready_buy, S}.

partial_short({sell, Price}, #state{pid_exec_conn=SendTo, pid_event=PidTo, count=Count, table_id = TableId} = S) ->
    Pos = position(TableId),
    if Pos =/= -?LIMIT ->
	    OrderQty = Pos + ?LIMIT,
	    gen_event:notify(PidTo,{new_order_single, SendTo, Count, sell, OrderQty,[{ord_type,2},{price, Price}]}),
	    {next_state, selling2short,
	     S#state{count = Count + 1, order_count = Count, ord_qty = OrderQty, price = Price, side = 'sell', order_state = placed}};
       true ->
	    {next_state, short, S}
    end;
partial_short({buy, Price}, #state{pid_exec_conn=SendTo, pid_event=PidTo, count=Count, table_id = TableId} = S) ->
    Pos = position(TableId),
    if Pos =/= -?LIMIT ->
	    OrderQty = ?LIMIT - Pos,
	    gen_event:notify(PidTo,{new_order_single, SendTo, Count, buy, OrderQty,[{ord_type,2},{price, Price}]}),
	    {next_state, buying2long,
	     S#state{count = Count + 1, order_count = Count, ord_qty = OrderQty, price = Price, side = 'buy', order_state = placed}};
       true ->
	    {next_state, short, S}
    end;
partial_short(break_conn, #state{} = S) ->
    {next_state, idle, S#state{state_name = partial_short}};
partial_short(_Event, S) ->
    {next_state, partial_short, S}.

partial_long({buy, Price}, #state{pid_exec_conn=SendTo, pid_event=PidTo, count=Count, table_id = TableId} = S) ->
    Pos = position(TableId),
    if Pos =/= ?LIMIT ->
	    OrderQty = -Pos + ?LIMIT,
	    gen_event:notify(PidTo,{new_order_single, SendTo, Count, buy, OrderQty,[{ord_type,2},{price, Price}]}),
	    {next_state, buying2long,
	     S#state{count = Count + 1, order_count = Count, ord_qty = OrderQty, price = Price, side = 'buy', order_state = placed}};
       true ->
	    {next_state, long, S}
    end;
partial_long({sell, Price}, #state{pid_exec_conn=SendTo, pid_event=PidTo, count=Count, table_id = TableId} = S) ->
    Pos = position(TableId),
    if Pos =/= ?LIMIT ->
	    OrderQty = Pos + ?LIMIT,
	    gen_event:notify(PidTo,{new_order_single, SendTo, Count, sell, OrderQty,[{ord_type,2},{price, Price}]}),
	    {next_state, selling2short,
	     S#state{count = Count + 1, order_count = Count, ord_qty = OrderQty, price = Price, side = 'sell', order_state = placed}};
       true ->
	    {next_state, long, S}
    end;
partial_long(break_conn, #state{} = S) ->
    {next_state, idle, S#state{state_name = partial_long}};
partial_long(_E, S) ->
    {next_state, partial_long, S}.

selling2cash({filled, Qty, Side}, #state{table_id = TableId} = S) ->
    orders_book({filled, Qty, Side, TableId}),
    ?D({filled, Qty}),
    {next_state, sell_cash, S#state{order_state = filled}};
selling2cash(expired, #state{} = S) ->
    {next_state, partial_short, S#state{order_state = expired}};
selling2cash({partial, Qty, Side}, #state{table_id = TableId} = S) ->
    orders_book({partial, Qty, Side, TableId}),
    ?D({partial, Qty, Side}),
    {next_state, selling2cash, S#state{order_state = partial}};
selling2cash({sell, Price},
	     #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count,
		    order_count = Order_Count, ord_qty = OrderQty, side = Side} = S) ->
    ?D({sell_to_cash, Price}),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, Side, OrderQty, [{ord_type,2}]}),
    {next_state, canceled_selling,
     S#state{count = Count + 1, price = Price, order_state = canceled}};
selling2cash(break_conn, #state{} = S) ->
    {next_state, idle, S#state{state_name = selling_cash}};
selling2cash(_E, S) ->
    {next_state, selling2cash, S}.

buying2cash({filled, Qty, Side}, #state{table_id = TableId} = S) ->
    orders_book({filled, Qty, Side, TableId}),
    ?D({filled, Qty, Side}),
    {next_state, buy_cash, S#state{order_state = filled}};
buying2cash(expired, #state{} = S) ->
    {next_state, partial_long, S#state{order_state = expired}};
buying2cash({partial, Qty, Side}, #state{table_id = TableId} = S) ->
    orders_book({partial, Qty, Side, TableId}),
    ?D({partial, Qty, Side}),
    {next_state, buying2cash, S#state{order_state = partial}};
buying2cash({buy, Price},
	    #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count,
		   order_count = Order_Count, ord_qty = OrderQty, side = Side} = S) ->
    ?D({buy_to_cash, Price}),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, Side, OrderQty, [{ord_type,2}]}),
    {next_state, canceled_buying,
     S#state{count = Count + 1, price = Price, order_state = canceled}};
buying2cash(break_conn, #state{} = S) ->
    {next_state, idle, S#state{state_name = buying_cash}};
buying2cash(_E, S) ->
    {next_state, buying2cash, S}.

sell_cash({sell, Price}, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count} = S) ->
    ?D({sell_from_cash_at, Price}),
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, sell, ?LIMIT, [{ord_type,2},{price, Price}]}),
    {next_state, selling2short,
     S#state{count = Count + 1, order_count = Count, ord_qty = ?LIMIT, price = Price, side = 'sell', order_state = placed}};
sell_cash(break_conn, #state{} = S) ->
    {next_state, idle, S#state{state_name = sell_cash}};
sell_cash(_E, S) ->
    {next_state, sell_cash, S}.

buy_cash({buy, Price}, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count} = S) ->
    ?D({buy_from_cash_at, Price}),
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, buy, ?LIMIT, [{ord_type,2},{price, Price}]}),
    {next_state, buying2long,
     S#state{count = Count + 1, order_count = Count, ord_qty = ?LIMIT, price = Price, side = 'buy', order_state = placed}};
buy_cash(break_conn, #state{} = S) ->
    {next_state, idle, S#state{state_name = buy_cash}};
buy_cash(_E, S) ->
    {next_state, buy_cash, S}.

canceled_to_market_buying({canceled, Qty, Order_Side}, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count,
					   price = Price, table_id = TableId} = S) ->
    ?D(canceled_buying_limit),
    Pos = position(TableId),
    if Pos < 0 ->
	    OrderQty = -Pos,
	    gen_event:notify(PidTo, {new_order_single, SendTo, Count, buy, OrderQty, [{ord_type,2},{price, Price}]}),
	    orders_book({canceled, Qty, Order_Side, TableId}),
	    {next_state, buy_market,
	     S#state{count = Count + 1, order_count = Count, ord_qty = OrderQty, side = 'buy', order_state = placed}};
       true ->
	    orders_book({canceled, Qty, Order_Side, TableId}),
	    {next_state, buy_cash, S}
    end;
canceled_to_market_buying(executed, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count,
					   price = Price, table_id = TableId, cncl_ord_side = Cncl_Side, cncl_qty = Cncl_Qty} = S) ->
    ?D(canceled_buying_limit),
    Pos = case ets:lookup(TableId, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  0
	  end,
    if Pos < 0 ->
	    OrderQty = -1 * Pos,
	    gen_event:notify(PidTo, {new_order_single, SendTo, Count, buy, OrderQty, [{ord_type,2},{price, Price}]}),
	    orders_book({executed, Cncl_Qty, Cncl_Side, TableId}),
	    {next_state, buy_market,
	     S#state{count = Count + 1, order_count = Count, ord_qty = OrderQty, side = 'buy', order_state = placed}};
       true ->
	    orders_book({executed, Cncl_Qty, Cncl_Side, TableId}),
	    {next_state, buy_cash, S}
    end;
canceled_to_market_buying(replaced, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count,
					   price = Price, table_id = TableId, cncl_ord_side = Cncl_Side, cncl_qty = Cncl_Qty} = S) ->
    ?D(canceled_buying_limit),
    Pos = case ets:lookup(TableId, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  0
	  end,
    if Pos < 0 ->
	    OrderQty = -1 * Pos,
	    gen_event:notify(PidTo, {new_order_single, SendTo, Count, buy, OrderQty, [{ord_type,2},{price, Price}]}),
	    orders_book({replaced, Cncl_Qty, Cncl_Side, TableId}),
	    {next_state, buy_market,
	     S#state{count = Count + 1, order_count = Count, ord_qty = OrderQty, side = 'buy', order_state = placed}};
       true ->
	    orders_book({replaced, Cncl_Qty, Cncl_Side, TableId}),
	    {next_state, buy_cash, S}
    end;
canceled_to_market_buying(unknown, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count,
					   price = Price, table_id = TableId, cncl_ord_side = Cncl_Side, cncl_qty = Cncl_Qty} = S) ->
    ?D(canceled_buying_limit),
    Pos = case ets:lookup(TableId, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  0
	  end,
    if Pos < 0 ->
	    OrderQty = -1 * Pos,
	    gen_event:notify(PidTo, {new_order_single, SendTo, Count, buy, OrderQty, [{ord_type,2},{price, Price}]}),
	    orders_book({unknown, Cncl_Qty, Cncl_Side, TableId}),
	    {next_state, buy_market,
	     S#state{count = Count + 1, order_count = Count, ord_qty = OrderQty, side = 'buy', order_state = placed}};
       true ->
	    orders_book({unknown, Cncl_Qty, Cncl_Side, TableId}),
	    {next_state, buy_cash, S}
    end;
canceled_to_market_buying(other, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count,
					   price = Price, table_id = TableId, cncl_ord_side = Cncl_Side, cncl_qty = Cncl_Qty} = S) ->
    ?D(canceled_buying_limit),
    Pos = case ets:lookup(TableId, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  0
	  end,
    if Pos < 0 ->
	    OrderQty = -1 * Pos,
	    gen_event:notify(PidTo, {new_order_single, SendTo, Count, buy, OrderQty, [{ord_type,2},{price, Price}]}),
	    orders_book({other, Cncl_Qty, Cncl_Side, TableId}),
	    {next_state, buy_market,
	     S#state{count = Count + 1, order_count = Count, ord_qty = OrderQty, side = 'buy', order_state = placed}};
       true ->
	    orders_book({other, Cncl_Qty, Cncl_Side, TableId}),
	    {next_state, buy_cash, S}
    end;
canceled_to_market_buying(break_conn, #state{} = S) ->
    {next_state, idle, S#state{state_name = canceled_to_market_buying}};
canceled_to_market_buying(_E, S) ->
    {next_state, canceled_to_market_buying, S}.

canceled_to_market_selling({canceled, Qty, Order_Side}, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count,
					    price = Price, table_id = TableId} = S) ->
    ?D(canceled_selling_limit_short),
    Pos = position(TableId),
    if Pos > 0 ->
	    OrderQty = Pos,
	    gen_event:notify(PidTo, {new_order_single, SendTo, Count, sell, OrderQty, [{ord_type,2},{price, Price}]}),
	    orders_book({canceled, Qty, Order_Side, TableId}),
	    {next_state, sell_market,
	     S#state{count = Count + 1, order_count = Count, ord_qty = OrderQty, side = 'sell', order_state = placed}};
       true ->
	    orders_book({canceled, Qty, Order_Side, TableId}),
	    {next_state, sell_cash, S}
    end;
canceled_to_market_selling(executed, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count,
					    price = Price, table_id = TableId, cncl_ord_side = Cncl_Side, cncl_qty = Cncl_Qty} = S) ->
    ?D(canceled_selling_limit_short),
    Pos = case ets:lookup(TableId, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  0
	  end,
    if Pos > 0 ->
	    OrderQty = Pos,
	    gen_event:notify(PidTo, {new_order_single, SendTo, Count, sell, OrderQty, [{ord_type,2},{price, Price}]}),
	    orders_book({executed, Cncl_Qty, Cncl_Side, TableId}),
	    {next_state, sell_market,
	     S#state{count = Count + 1, order_count = Count, ord_qty = OrderQty, side = 'sell', order_state = placed}};
       true ->
	    orders_book({executed, Cncl_Qty, Cncl_Side, TableId}),
	    {next_state, sell_cash, S}
    end;
canceled_to_market_selling(replaced, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count,
					    price = Price, table_id = TableId, cncl_ord_side = Cncl_Side, cncl_qty = Cncl_Qty} = S) ->
    ?D(canceled_selling_limit_short),
    Pos = case ets:lookup(TableId, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  0
	  end,
    if Pos > 0 ->
	    OrderQty = Pos,
	    gen_event:notify(PidTo, {new_order_single, SendTo, Count, sell, OrderQty, [{ord_type,2},{price, Price}]}),
	    orders_book({replaced, Cncl_Qty, Cncl_Side, TableId}),
	    {next_state, sell_market,
	     S#state{count = Count + 1, order_count = Count, ord_qty = OrderQty, side = 'sell', order_state = placed}};
       true ->
	    orders_book({replaced, Cncl_Qty, Cncl_Side, TableId}),
	    {next_state, sell_cash, S}
    end;
canceled_to_market_selling(unknown, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count,
					    price = Price, table_id = TableId, cncl_ord_side = Cncl_Side, cncl_qty = Cncl_Qty} = S) ->
    ?D(canceled_selling_limit_short),
    Pos = case ets:lookup(TableId, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  0
	  end,
    if Pos > 0 ->
	    OrderQty = Pos,
	    gen_event:notify(PidTo, {new_order_single, SendTo, Count, sell, OrderQty, [{ord_type,2},{price, Price}]}),
	    orders_book({unknown, Cncl_Qty, Cncl_Side, TableId}),
	    {next_state, sell_market,
	     S#state{count = Count + 1, order_count = Count, ord_qty = OrderQty, side = 'sell', order_state = placed}};
       true ->
	    orders_book({unknown, Cncl_Qty, Cncl_Side, TableId}),
	    {next_state, sell_cash, S}
    end;
canceled_to_market_selling(other, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count,
					    price = Price, table_id = TableId, cncl_ord_side = Cncl_Side, cncl_qty = Cncl_Qty} = S) ->
    ?D(canceled_selling_limit_short),
    Pos = case ets:lookup(TableId, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  0
	  end,
    if Pos > 0 ->
	    OrderQty = Pos,
	    gen_event:notify(PidTo, {new_order_single, SendTo, Count, sell, OrderQty, [{ord_type,2},{price, Price}]}),
	    orders_book({other, Cncl_Qty, Cncl_Side, TableId}),
	    {next_state, sell_market,
	     S#state{count = Count + 1, order_count = Count, ord_qty = OrderQty, side = 'sell', order_state = placed}};
       true ->
	    orders_book({other, Cncl_Qty, Cncl_Side, TableId}),
	    {next_state, sell_cash, S}
    end;
canceled_to_market_selling(break_conn, #state{} = S) ->
    {next_state, idle, S#state{state_name = canceled_to_market_selling}};
canceled_to_market_selling(_Event, S) ->
    {next_state, canceled_selling, S}.

sell_market(rejected, #state{} = S) ->
    ?D(rejected),
    {next_state, sell_market, S#state{order_state = rejected}};
sell_market(expired, #state{} = S) ->
    {next_state, sell_market, S#state{order_state = expired}};
sell_market({filled, Qty, Side}, #state{table_id = TableId} = S) ->
    orders_book({filled, Qty, Side, TableId}),
    ?D({filled, Qty, Side}),
    {next_state, sell_cash, S#state{order_state = filled}};
sell_market({partial, Qty, Side}, #state{table_id = TableId} = S) ->
    orders_book({partial, Qty, Side, TableId}),
    ?D({partial, Qty, TableId}),
    {next_state, sell_market, S#state{order_state = partial, partial_qty = Qty}};
sell_market({market_sell, Price}, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, order_count = Order_Count,
					 ord_qty = OrderQty, side = Side} = S) ->
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, Side, OrderQty, [{ord_type,2}]}),
    {next_state, canceled_to_market_selling,
     S#state{count = Count + 1, price = Price, order_state = placed}};
sell_market(break_conn, #state{} = S) ->
    {next_state, idle, S#state{state_name = sell_market}};
sell_market(_E, S) ->
    {next_state, sell_market, S}.

buy_market(rejected, #state{} = S) ->
    ?D(rejected),
    {next_state, buy_market, S#state{order_state = rejected}};
buy_market(expired, #state{} = S) ->
    {next_state, buy_market, S#state{order_state = expired}};
buy_market({filled, Qty, Side}, #state{table_id = TableId} = S) ->
    orders_book({filled, Qty, Side, TableId}),
    ?D({filled, Qty, Side, TableId}),
    {next_state, buy_cash, S#state{order_state = filled}};
buy_market({partial, Qty, Side}, #state{table_id = TableId} = S) ->
    orders_book({partial, Qty, Side, TableId}),
    ?D({partial, Qty, Side, TableId}),
    {next_state, buy_market, S#state{order_state = partial, partial_qty = Qty}};
buy_market({market_buy, Price}, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, order_count = Order_Count,
				       ord_qty = OrderQty, side = Side} = S) ->
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, Side, OrderQty, [{ord_type,2}]}),
    {next_state, canceled_to_market_buying,
     S#state{count = Count + 1, price = Price, order_state = placed}};
buy_market(break_conn, #state{} = S) ->
    {next_state, idle, S#state{state_name = buy_market}};
buy_market(_E, S) ->
    {next_state, buy_market, S}.

handle_event(close, _StateName, S = #state{}) ->
    check_orders(S),
    check_positions(S),
    {next_state, closing, S#state{order_state = canceled}};
handle_event(shutdown, _StateName, State) ->
    {stop, normal, State};
handle_event(Event, StateName, State) ->
    io:format("Receives an unknown global sync event: ~p ~n", [Event]),
    {next_state, StateName, State}.

handle_sync_event(Event, _From, StateName, State) ->
    io:format("Receives an unknown global sync event: ~p ~n", [Event]),
    {reply, "wait",  StateName, State}.

handle_info({'ETS-TRANSFER', TableId, Pid, _Data}, StateName, #state{} = Data) ->
    io:format("MGR(~p) -> FSM(~p) getting TableId: ~p~n", [Pid, self(), TableId]),
    {next_state, StateName, Data#state{table_id = TableId}};
handle_info(_Info, StateName, Data) ->
    {next_state, StateName, Data}.

terminate(normal, _State, #state{order_state = Order_State, table_id = TableId} = S) ->
    case Order_State of
	placed ->
	    check_orders(S);
	partial ->
	    check_orders(S);
	_ -> 
	    ok
    end,
    Pos = case ets:lookup(TableId, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  0
	  end,
    case Pos of
	0 ->
	    ok;
	_ ->
	    check_positions(S)
    end,
    ets:delete(orders),
    gen_event:delete_handler(S#state.pid_event, fast_msg_sender, []),
    ok;
terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

closing(stop, S) ->
    {stop, normal, S};
closing(_E, S) ->
    {next_state, closing, S}.

orders_book({OrdStatus, CQ, Side, TableId}) ->
    Pos = case ets:lookup(TableId, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  0
	  end,
    Prev_Status = case ets:lookup(TableId, rep) of
		      [{_, Prev_Stat, _, _}] ->
			  Prev_Stat;
		      _ ->
			  0
		  end,
    Prev_CQ = case ets:lookup(TableId, rep) of
		   [{_, _, Prev_CQtys, _}] ->
		       Prev_CQtys;
		   _ ->
		       0
	       end,
    Prev_Side = case ets:lookup(TableId, rep) of
		     [{_, _, _, Prev_Sides}] ->
			 Prev_Sides;
		     _ ->
			 0
		 end,
    Partial = case OrdStatus of
		  filled when Prev_Status == partial ->
		      case Side of
			  sell when Prev_Side == sell ->
			      Pos - CQ + Prev_CQ;   
			  buy when Prev_Side == buy ->
			      Pos + CQ - Prev_CQ;
			  sell ->
			      Pos - CQ;
			  buy ->
			      Pos + CQ
		      end;
		  filled ->
		      case Side of
			  sell ->
			      Pos - CQ;
			  buy ->
			      Pos + CQ;
			  _ -> ok
		      end;
		  partial when Prev_Status == partial ->
		      case Side of
			  sell when Prev_Side == sell ->
			      Pos - CQ + Prev_CQ;
			  sell ->
			      Pos - CQ;
			  buy when Prev_Side == buy ->
			      Pos + CQ - Prev_CQ;
			  buy ->
			      Pos + CQ;
			  _ -> ok
		      end;
		  partial ->
		      case Side of
			  sell ->
			      Pos - CQ;
			  buy ->
			      Pos + CQ;
			  _ -> ok
		      end;
		  rejected ->
		      Pos;
		  canceled ->
		      Pos;
		  expired ->
		      Pos;
		  replaced ->
		      Pos;
		  unknown ->
		      Pos;
		  other ->
		      Pos;

		  _ -> 
		      ok
	      end,		
    ?D(Partial),
    ets:insert(TableId, {rep, OrdStatus, CQ, Side}),
    ets:insert(TableId, {pos, Partial}).

check_orders(#state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, order_count = Order_Count,
			ord_qty = OrderQty, partial_qty = Qty, side = Side, order_state = Order_State}) ->
    case Order_State of
	placed ->
	    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, Side, OrderQty, [{ord_type,2}]});
	partial ->
	    Leaves_Qty = OrderQty - Qty,
	    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, Side, Leaves_Qty, [{ord_type,2}]});
	_ ->
	    ok
    end.

check_positions(#state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, price = Price, table_id = TableId}) ->
    Pos = case ets:lookup(TableId, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  0
	  end,
    if Pos > 0 ->
%	    OrderQty = Pos,
	    Exp_Price = Price - 1000,
	    ?D({exp_price, Exp_Price}),
	    gen_event:notify(PidTo, {new_order_single, SendTo, Count, sell, Pos, [{ord_type,2},{price, Exp_Price}]});
       Pos < 0 ->
%	    OrderQty = Pos,
	    Exp_Price = Price + 1000,
	    ?D({exp_price, Exp_Price}),
	    gen_event:notify(PidTo, {new_order_single, SendTo, Count, buy, -Pos, [{ord_type,2},{price, Exp_Price}]});
       true ->
	    ok
    end.

position(TableId) ->
   case ets:lookup(TableId, pos) of
	[{_, Poss}] ->
		  Poss;
	_ ->
		  0
   end.
