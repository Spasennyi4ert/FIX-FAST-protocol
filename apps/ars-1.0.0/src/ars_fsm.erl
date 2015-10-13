-module(ars_fsm).
-behaviour(gen_fsm).

-include("log.hrl").

-define(LIMIT, 5).

-record(state, {
	  pid_exec_conn,
	  pid_event,
	  count = 1,
	  order_qty = 0,
	  partial_qty = 0,
	  price = 0,
	  order_count = 0,
	  side,
	  order_state
}).

%% public API
-export([start_link/0, terminate/3, code_change/4, handle_info/3, handle_event/3, handle_sync_event/4]).

%% callbacks
-export([init/1]).

%% custom state names
-export([wait/2, sell_market/2, buy_market/2, buy_cash/2, sell_cash/2, canceled_to_market_selling/2, partial_short/2,
	 partial_long/2, canceled_to_market_buying/2, long/2, short/2, ready_to_sell/1, ready_to_buy/1, orders_book/1, buying/2,
	canceled_buying/2, selling/2, canceled_selling/2, selling_cash/2, buying_cash/2]).

% events
-export([buy/2, sell/2, repeat_up/2, repeat_down/2, rejected/1, expired/1, close/1, closing/2,
	  ready_sell/2, ready_buy/2, market_sell/2, market_buy/2, filled/2, partial/2, canceled/1]).
% privat
%-export([connect/3]).

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

buy( Pid, Number) ->
    gen_fsm:send_event( Pid, {buy, Number}).

sell( Pid, Number) ->
    gen_fsm:send_event( Pid, {sell, Number}).

ready_to_sell( Pid) ->
    gen_fsm:send_event( Pid, ready_to_sell).

market_sell( Pid,  Number) ->
    gen_fsm:send_event( Pid, {market_sell, Number}).

ready_to_buy( Pid) ->
    gen_fsm:send_event( Pid, ready_to_buy).

market_buy( Pid,  Number) ->
    gen_fsm:send_event( Pid, {market_buy, Number}).

repeat_up(Pid, Number) ->
    gen_fsm:send_event(Pid, {repeat_up, Number}).

repeat_down(Pid, Number) ->
    gen_fsm:send_event(Pid, {repeat_down, Number}).

partial(Pid, Qty) ->
    gen_fsm:send_event(Pid, {partial, Qty}).

filled(Pid, Qty) ->
    gen_fsm:send_event(Pid, {filled, Qty}).

canceled(Pid) ->
    gen_fsm:send_event(Pid, canceled).

rejected(Pid) ->
    gen_fsm:send_event(Pid, rejected).

expired(Pid) ->
    gen_fsm:send_event(Pid, expired).

close(Pid) ->
    gen_fsm:send_all_state_event(Pid, close).

init([]) ->
    ars:start_service(handler, {ars_handler, start_link, []}),
    ars:run_task(handler, [self()]),
    {ok, PidTo} = gen_event:start_link(),
    gen_event:add_handler(PidTo, ars_msg_sender, []),
%    connect(PidTo, SendTo, self()),
    ets:new(orders, [named_table, set, public]),
    {ok, wait, #state{pid_event = PidTo}}.

terminate(normal, _State, S) ->
    case S#state.order_state of
	placed ->
	    check_orders(S);
	partial ->
	    check_orders(S);
	_ -> 
	    ok
    end,
    Pos = case ets:lookup(orders, pos) of
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

handle_info(_Info, StateName, Data) ->
    {next_state, StateName, Data}.

orders_book({OrdStatus, LQ, CQ, Side}) ->
    Pos = case ets:lookup(orders, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  0
	  end,
    Prev_Status = case ets:lookup(orders, rep) of
		      [{_, Prev_Stat, _, _, _}] ->
			  Prev_Stat;
		      _ ->
			  0
		  end,
    Prev_CQ = case ets:lookup(orders, rep) of
		   [{_, _, _, Prev_CQtys, _}] ->
		       Prev_CQtys;
		   _ ->
		       0
	       end,
    Prev_Side = case ets:lookup(orders, rep) of
		     [{_, _, _, _, Prev_Sides}] ->
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
		  _ -> 
		      ok
	      end,		
    ?D(Partial),
    ets:insert(orders, {rep, OrdStatus, LQ, CQ, Side}),
    ets:insert(orders, {pos, Partial}).

wait({buy, Price}, #state{pid_event=PidTo, count=Count} = S) ->
    ?D({buy_at, Price}),
    SendTo = whereis(fix_read),
    OrderQty = ?LIMIT,
	?D(SendTo),
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, buy, OrderQty, [{ord_type,2},{price,Price}]}),
	?D(OrderQty),
    {next_state, buying,
     S#state{pid_exec_conn=SendTo, count=Count+1, order_count=Count, order_qty=OrderQty, price=Price, side='buy', order_state = placed}};
wait({sell, Price}, S = #state{pid_event=PidTo, count=Count}) ->
    ?D({sell_at, Price}),
    SendTo = whereis(fix_read),
    OrderQty = ?LIMIT,
	?D(SendTo),
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, sell, OrderQty, [{ord_type,2},{price, Price}]}),
	?D(OrderQty),
    {next_state, selling,
     S#state{pid_exec_conn=SendTo, count=Count+1, order_count=Count, order_qty=OrderQty, price=Price, side='sell', order_state = placed}};
wait(_Event, S) ->
    {next_state, wait, S}.

buying(rejected, S = #state{}) ->
    ?D(rejected),
    {next_state, buy_cash, S#state{order_state = rejected}};
buying(expired, S = #state{}) ->
    {next_state, buy_cash, S#state{order_state = expired}};
buying({filled, Qty}, S = #state{}) ->
    ?D({filled, Qty}),
    {next_state, long, S#state{order_state = filled}};
buying({partial, Qty}, S = #state{}) ->
    ?D({partial, Qty}),
    {next_state, buying, S#state{order_state = partial, partial_qty = Qty}};
buying({repeat_up, Price},
       S = #state{pid_exec_conn=SendTo, pid_event=PidTo, count=Count, order_count=Order_Count, order_qty=OrderQty, side = Side}) ->
    ?D({repeat_buy_at, Price}),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, Side, OrderQty, [{ord_type,2}]}),
    {next_state, canceled_buying, S#state{count = Count + 1, price = Price, order_state = canceled}};
buying({sell, Price},
       S = #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, order_count = Order_Count, order_qty = OrderQty, side = Side}) ->
    ?D({double_sell_at, Price, from_partial}),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, Side, OrderQty, [{ord_type,2}]}),
    {next_state, canceled_selling, S#state{count = Count + 1, price = Price, side = 'sell', order_state = canceled}};
buying(ready_to_sell,
       S = #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, order_count = Order_Count, order_qty = OrderQty, side = Side}) ->
    ?D(ready_to_sell_from_partial),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, Side, OrderQty, [{ord_type,2}]}),
    {next_state, ready_sell, S#state{count = Count + 1, order_state = canceled}};
buying({market_sell, Price},
       S = #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, order_count = Order_Count, order_qty = OrderQty, side = Side}) ->
    ?D({market_sell_at, Price}),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, Side, OrderQty, [{ord_type,2}]}),
    {next_state, canceled_to_market_selling, S#state{count = Count + 1, price = Price, order_state = canceled}};
buying(_Event, S) ->
    {next_state, buying, S}.

selling(rejected, S = #state{}) ->
    ?D(rejected),
    {next_state, sell_cash, S#state{order_state = rejected}};
selling(expired, S = #state{}) ->
    {next_state, sell_cash, S#state{order_state = expired}};
selling({filled, Qty}, S = #state{}) ->
    ?D({filled, Qty}),
    {next_state, short, S#state{order_state = filled}};
selling({partial, Qty}, S = #state{}) ->
    ?D({partial, Qty}),
    {next_state, selling, S#state{order_state = partial, partial_qty = Qty}};
selling({repeat_down, Price},
	S = #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, order_count = Order_Count, order_qty = OrderQty, side = Side}) ->
    ?D({repeat_sell_at, Price}),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, Side, OrderQty, [{ord_type,2}]}),
    {next_state, canceled_selling, S#state{count = Count + 1, price = Price, order_state = canceled}};
selling({buy, Price},
	S = #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, order_count = Order_Count, order_qty = OrderQty, side = Side}) ->
    ?D({buy_at, Price, from_partial}),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, Side, OrderQty, [{ord_type,2}]}),
    {next_state, canceled_buying, S#state{count = Count + 1, price = Price, side = 'buy', order_state = canceled}};
selling(ready_to_buy,
	S = #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, order_count = Order_Count, order_qty = OrderQty, side = Side}) ->
    ?D(ready_to_buy_from_partial),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, Side, OrderQty, [{ord_type,2}]}),
    {next_state, ready_buy, S#state{count = Count + 1}};
selling({market_buy, Price},
       #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, order_count = Order_Count, order_qty = OrderQty, side = Side} = S) ->
    ?D({market_sell_at, Price}),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, Side, OrderQty, [{ord_type,2}]}),
    {next_state, canceled_to_market_buying, S#state{count = Count + 1, price = Price, order_state = canceled}};
selling(_Event, S) ->
    {next_state, selling, S}.

canceled_buying(canceled, S = #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, price = Price, side = Side}) ->
    ?D(canceled_buying_limit),
    Pos = case ets:lookup(orders, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  0
	  end,
    if Pos < ?LIMIT ->
	    OrderQty = ?LIMIT - Pos,
	    gen_event:notify(PidTo, {new_order_single,SendTo,Count, Side, OrderQty, [{ord_type,2},{price, Price}]}),
	    {next_state, buying, S#state{count = Count + 1, order_count = Count, order_qty = OrderQty, order_state = placed}};
       Pos >= ?LIMIT ->
	    {next_state, long, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count}}
    end;
canceled_buying(_Event, S) ->
    {next_state, canceled_buying, S}.

canceled_selling(canceled, S = #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, price = Price, side = Side}) ->
    ?D(canceled_selling_limit_short),
    Pos = case ets:lookup(orders, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  0
	  end,
    
    if Pos > -?LIMIT ->
	    OrderQty = ?LIMIT + Pos,
	    gen_event:notify(PidTo,{new_order_single, SendTo, Count, Side, OrderQty, [{ord_type,2},{price, Price}]}),
	    {next_state, selling, S#state{count = Count + 1, order_count = Count, order_qty = OrderQty, order_state = placed}};
       Pos =< -?LIMIT ->
	    {next_state, short, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count}}
    end;
canceled_selling(_Event, State) ->
    {next_state, canceled_selling, State}.

long({sell, Price}, S = #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count}) ->
    ?D({sell_at, Price}),
    OrderQty = 2*?LIMIT,
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, sell, OrderQty, [{ord_type,2},{price, Price}]}),
    {next_state, selling,
     S#state{count = Count + 1, order_count = Count, order_qty = OrderQty, price = Price, side = 'sell', order_state = placed}};
long(ready_to_sell, S) ->
    ?D(ready_to_sell),
    {next_state, ready_sell, S};
long({market_sell, Price}, S = #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count}) ->
    ?D({sell_market_at, Price}),
    OrderQty = ?LIMIT,
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, sell, OrderQty, [{ord_type,2},{price, Price}]}),
    {next_state, sell_cash,
     S#state{count = Count + 1, order_count = Count, order_qty = OrderQty, price = Price, side = 'sell', order_state = placed}};
long(_Event, S) ->
    {next_state, long, S}.

short({buy, Price}, S = #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count}) ->
    ?D({buy_at, Price}),
    OrderQty = 2*?LIMIT,
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, buy, OrderQty, [{ord_type,2},{price, Price}]}),
    {next_state, buying,
     S#state{count = Count + 1, order_count = Count, order_qty = OrderQty, price = Price, side = 'buy', order_state = placed}};
short(ready_to_buy, S) ->
    ?D(ready_to_buy),
    {next_state, ready_buy, S};
short({market_buy, Price}, S = #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count}) ->
    ?D({buy_market_at, Price}),
    OrderQty = ?LIMIT,
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, buy, OrderQty, [{ord_type,2},{price, Price}]}),
    {next_state, buy_cash,
     S#state{count = Count + 1, order_count = Count, order_qty = OrderQty, price = Price, side = 'buy', order_state = placed}};
short(_Event, S) ->
    {next_state, short, S}.

ready_sell({sell, Price}, S = #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count}) ->
    ?D({ready_sell_at, Price}),
    Pos = case ets:lookup(orders, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  0
	  end,
    if Pos > 0 ->
	    OrderQty = Pos,
	    gen_event:notify(PidTo,{new_order_single, SendTo, Count, sell, OrderQty,[{ord_type,2},{price, Price}]}),
	    {next_state, selling_cash,
	     S#state{count = Count + 1, order_count = Count, order_qty = OrderQty, price = Price, side = 'sell', order_state = placed}};
       Pos == -?LIMIT ->
	    {next_state, short, S};
       Pos == 0 ->
	    {next_state, sell_cash, S};
       true ->
	    OrderQty = ?LIMIT + Pos,
	    {next_state, partial_short, S#state{order_qty = OrderQty}}
    end;
ready_sell(_Event, S) ->
    {next_state, ready_sell, S}.

ready_buy({buy, Price}, S = #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count}) ->
    ?D({ready_buy_at, Price}),
    Pos = case ets:lookup(orders, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  0
	  end,
    if Pos < 0 ->
	    OrderQty = -Pos,
	    gen_event:notify(PidTo,{new_order_single, SendTo, Count, buy, OrderQty,[{ord_type,2},{price, Price}]}),
	    {next_state, buying_cash,
	     S#state{count = Count + 1, order_count = Count, order_qty = OrderQty, price = Price, side = 'buy', order_state = placed}};
       Pos == ?LIMIT ->
	    {next_state, long, S};
       Pos == 0 ->
	    {next_state, buy_cash, S};
       true ->
	    OrderQty = ?LIMIT - Pos,
	    {next_state, partial_long, S#state{order_qty = OrderQty}}
    end;
ready_buy(_Event, S) ->
    {next_state, ready_buy, S}.

partial_short({sell, Price}, S = #state{pid_exec_conn=SendTo, pid_event=PidTo, count=Count, order_qty = OrderQty}) ->
    gen_event:notify(PidTo,{new_order_single, SendTo, Count, sell, OrderQty, [{ord_type,2},{price, Price}]}),
    {next_state, selling,
     S#state{count = Count + 1, order_count = Count, price = Price, side = 'sell', order_state = placed}};
partial_short(_Event, S) ->
    {next_state, partial_short, S}.

partial_long({buy, Price}, S = #state{pid_exec_conn=SendTo, pid_event=PidTo, count=Count, order_qty = OrderQty}) ->
    gen_event:notify(PidTo,{new_order_single, SendTo, Count, buy, OrderQty, [{ord_type,2},{price, Price}]}),
    {next_state, buying,
     S#state{count = Count + 1, order_count = Count, price = Price, side = 'buy', order_state = placed}};
partial_long(_Event, S) ->
    {next_state, partial_long, S}.

selling_cash({filled, Qty}, S = #state{}) ->
    ?D({filled, Qty}),
    {next_state, sell_cash, S#state{order_state = filled}};
selling_cash({expired, _Qty}, S = #state{}) ->
    {next_state, sell_cash, S#state{order_state = expired}};
selling_cash({partial, Qty}, S = #state{}) ->
    ?D({partial, Qty}),
    {next_state, selling_cash, S#state{order_state = partial, partial_qty = Qty}};
selling_cash({repeat_down, Price},
	     S = #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count,
			order_count = Order_Count, order_qty = OrderQty, side = Side}) ->
    ?D({sell_to_cash, Price}),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, Side, OrderQty, [{ord_type,2}]}),
    {next_state, canceled_selling,
     S#state{count = Count + 1, price = Price, order_state = canceled}};
selling_cash(_Event, S) ->
    {next_state, selling_cash, S}.

buying_cash({filled, Qty}, S = #state{}) ->
    ?D({filled, Qty}),
    {next_state, buy_cash, S#state{order_state = filled}};
buying_cash({expired, _Qty}, S = #state{}) ->
    {next_state, buy_cash, S#state{order_state = expired}};
buying_cash({partial, Qty}, S = #state{}) ->
    ?D({partial, Qty}),
    {next_state, buying_cash, S#state{order_state = partial, partial_qty = Qty}};
buying_cash({repeat_up, Price},
	    S = #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count,
		       order_count = Order_Count, order_qty = OrderQty, side = Side}) ->
    ?D({buy_to_cash, Price}),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, Side, OrderQty, [{ord_type,2}]}),
    {next_state, canceled_buying,
     S = #state{count = Count + 1, price = Price, order_state = canceled}};
buying_cash(_Event, S) ->
    {next_state, buying_cash, S}.

sell_cash({sell, Price}, S = #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count}) ->
    ?D({sell_from_cash_at, Price}),
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, sell, ?LIMIT, [{ord_type,2},{price, Price}]}),
    {next_state, selling,
     S#state{count = Count + 1, order_count = Count, order_qty = ?LIMIT, price = Price, side = 'sell', order_state = placed}};
sell_cash(_Event, S) ->
    {next_state, sell_cash, S}.

buy_cash({buy, Price}, S = #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count}) ->
    ?D({buy_from_cash_at, Price}),
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, buy, ?LIMIT, [{ord_type,2},{price, Price}]}),
    {next_state, buying,
     S#state{count = Count + 1, order_count = Count, order_qty = ?LIMIT, price = Price, side = 'buy', order_state = placed}};
buy_cash(_Event, S) ->
    {next_state, buy_cash, S}.

canceled_to_market_buying(canceled, S = #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, price = Price}) ->
    ?D(canceled_buying_limit),
    Pos = case ets:lookup(orders, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  0
	  end,
    if Pos < 0 ->
	    OrderQty = Pos,
	    gen_event:notify(PidTo, {new_order_single, SendTo, Count, buy, OrderQty, [{ord_type,2},{price, Price}]}),
	    {next_state, buy_market,
	     S#state{count = Count + 1, order_count = Count, order_qty = OrderQty, side = 'buy', order_state = placed}};
       true ->
	    {next_state, buy_cash, S}
    end;
canceled_to_market_buying(_Event, S) ->
    {next_state, canceled_to_market_buying, S}.

canceled_to_market_selling(canceled, S = #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, price = Price}) ->
    ?D(canceled_selling_limit_short),
    Pos = case ets:lookup(orders, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  0
	  end,
    if Pos > 0 ->
	    OrderQty = Pos,
	    gen_event:notify(PidTo, {new_order_single, SendTo, Count, sell, OrderQty, [{ord_type,2},{price, Price}]}),
	    {next_state, sell_market,
	     S#state{count = Count + 1, order_count = Count, order_qty = OrderQty, side = 'sell', order_state = placed}};
       true ->
	    {next_state, sell_cash, S}
    end;
canceled_to_market_selling(_Event, S) ->
    {next_state, canceled_selling, S}.

sell_market({market_sell, Price}, S = #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count}) ->
    ?D({sell_at, Price}),
    Pos = case ets:lookup(orders, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  0
	  end,
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, sell, Pos, [{ord_type,2},{price, Price}]}),
    {next_state, sell_cash,
     S#state{count = Count + 1, order_count = Count, order_qty = Pos, price = Price, side = 'sell', order_state = placed}};
sell_market(_Event, S) ->
    {next_state, sell_market, S}.

buy_market({market_buy, Price}, S = #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, price = Price}) ->
    ?D({buy_at, Price}),
    Pos = case ets:lookup(orders, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  0
	  end,
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, buy, Pos, [{ord_type,2},{price, Price}]}),
    {next_state, buy_cash,
     S#state{count = Count + 1, order_count = Count, order_qty = Pos, price = Price, side = 'buy', order_state = placed}};
buy_market(_Event, S) ->
    {next_state, buy_market, S}.

handle_event(close, _StateName, S = #state{}) ->
%	     S = #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, order_count = Order_Count,
%			order_qty = OrderQty, price = Price, side = Side, order_state = Order_State}) ->
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

closing(stop, S) ->
    {stop, normal, S};
closing(_Event, S) ->
    {next_state, closing, S}.

%connect(PidTo, SendTo, From) ->
%    gen_event:notify(PidTo, {connect, SendTo, From}).

check_orders(#state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, order_count = Order_Count,
			order_qty = OrderQty, partial_qty = Qty, side = Side, order_state = Order_State}) ->
    case Order_State of
	placed ->
	    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, Side, OrderQty, [{ord_type,2}]});
	partial ->
	    Leaves_Qty = OrderQty - Qty,
	    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, Side, Leaves_Qty, [{ord_type,2}]});
	_ ->
	    ok
    end.

check_positions(#state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, price = Price}) ->
    Pos = case ets:lookup(orders, pos) of
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
    
