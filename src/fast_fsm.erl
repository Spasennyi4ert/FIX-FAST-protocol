-module(fast_fsm).
-behaviour(gen_fsm).

-include("log.hrl").

-define(LIMIT, 5).

-record(state, {
	  pid_exec_conn,
	  pid_event,
	  count = 121,
	  order_qty = 120,
	  price = 0,
	  order_count = 0
}).

%% public API
-export([start_link/1]).

%% callbacks
-export([init/1, terminate/3, code_change/4, handle_info/3, handle_event/3, handle_sync_event/4]).

%% custom state names
-export([wait/2, sell_market/2, buy_market/2, buy_cash/2, sell_cash/2, canceled_to_market_selling/2,
	 canceled_to_market_buying/2, long/2, short/2, ready_to_sell/1, ready_to_buy/1, orders_book/1, buying/2,
	canceled_buying/2, selling/2, canceled_selling/2, selling_cash/2, buying_cash/2]).

% events
-export([ buy/2, sell/2, repeat_up/2, repeat_down/2, rejected/1,
	  ready_sell/2, ready_buy/2, market_sell/2, market_buy/2, filled/2, partial/2, canceled/1]).
% privat
-export([connect/3]).

start_link(SendTo) ->
    gen_fsm:start_link(?MODULE, [SendTo], []).

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

init([SendTo]) ->
    fast:start_task(fast_s, {fast_ind, start_link, []}),
    fast:run(fast_s, [self()]),
    {ok, PidTo} = gen_event:start_link(),
    gen_event:add_handler(PidTo, fast_msg_sender, []),
    connect(PidTo, SendTo, self()),
    ets:new(orders, [named_table, set, public]),
    {ok, wait, #state{pid_exec_conn = SendTo, pid_event = PidTo}}.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

handle_info(_Info, StateName, Data) ->
    {next_state, StateName, Data}.

orders_book({OrdStatus, Qty, Side}) ->
    Pos = case ets:lookup(orders, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  0
	  end,
    Prev_Status = case ets:lookup(orders, rep) of
		      [{_, Prev_Stat, _, _}] ->
			  Prev_Stat;
		      _ ->
			  empty
		  end,
    Prev_Qty = case ets:lookup(orders, rep) of
		   [{_, _, Prev_Qtys, _}] ->
		       Prev_Qtys;
		   _ ->
		       empty
	       end,
    Prev_Side = case ets:lookup(orders, rep) of
		     [{_, _, _, Prev_Sides}] ->
			 Prev_Sides;
		     _ ->
			 empty
		 end,
    Partial = case OrdStatus of
		  filled when Prev_Status == partial ->
		      case Side of
			  sell when Prev_Side == sell ->
			      Pos - Qty + Prev_Qty;
			  buy when Prev_Side == buy ->
			      Pos + Qty - Prev_Qty
		      end;
		  filled ->
		      case Side of
			  sell ->
			      Pos - Qty;
			  buy ->
			      Pos + Qty
		      end;
		  partial when Prev_Status == partial ->
		      case Side of
			  sell when Prev_Side == sell ->
			      Pos - Qty + Prev_Qty;
			  buy when Prev_Side == buy ->
			      Pos + Qty - Prev_Qty
		      end;
		  partial when Prev_Status == partial ->
		      case Side of
			  sell when Prev_Side =/= sell ->
			      Pos - Qty;
			  buy when Prev_Side =/= buy ->
			      Pos + Qty
		      end;
		  partial when Prev_Status =/= partial ->
		      case Side of
			  sell ->
			      Pos - Qty;
			  buy ->
			      Pos + Qty
		      end;
		  rejected ->
		      Pos
	      end,		
    ?D(Partial),
    ets:insert(orders, {rep, OrdStatus, Qty, Side}),
    ets:insert(orders, {pos, Partial}).


wait({buy, Price}, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count}) ->
    ?D({buy_at, Price}),
    OrderQty = ?LIMIT,
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, 'F.RIM5', buy, OrderQty, [{account, 'A80'},{ord_type,2},{price, Price}]}),
    {next_state, buying,
     #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count + 1, order_count = Count, order_qty = OrderQty, price = Price}};
wait({sell, Price}, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count}) ->
    ?D({sell_at, Price}),
    OrderQty = ?LIMIT,
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, 'F.RIM5', sell, OrderQty, [{account, 'A80'},{ord_type,2},{price, Price}]}),
    {next_state, selling,
     #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count + 1, order_count = Count, order_qty = OrderQty, price = Price}};
wait(_Event, State) ->
    {next_state, wait, State}.

buying(rejected, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count}) ->
    ?D(rejected),
    {next_state, buy_cash, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count}};
buying({filled, Qty}, #state{} = State) ->
    ?D({filled, Qty}),
    {next_state, long, State};
buying({partial, Qty}, #state{} = State) ->
    ?D({partial, Qty}),
    {next_state, buying, State};
buying({repeat_up, Price},
       #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, order_count = Order_Count, order_qty = OrderQty}) ->
    ?D({repeat_buy_at, Price}),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, 'F.RIM5', buy, OrderQty, [{account, 'A80'},{ord_type,2}]}),
    {next_state, canceled_buying, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count + 1, price = Price}};
buying({sell, Price},
       #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, order_count = Order_Count, order_qty = OrderQty}) ->
    ?D({double_sell_at, Price, from_partial}),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, 'F.RIM5', buy, OrderQty, [{account, 'A80'},{ord_type,2}]}),
    {next_state, canceled_selling,
     #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count + 1, price = Price}};
buying(ready_to_sell,
       #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, order_count = Order_Count, order_qty = OrderQty}) ->
    ?D(ready_to_sell_from_partial),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, 'F.RIM5', buy, OrderQty, [{account, 'A80'},{ord_type,2}]}),
    {next_state, ready_sell,
     #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count + 1}};
buying({market_sell, Price},
       #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, order_count = Order_Count, order_qty = OrderQty}) ->
    ?D({market_sell_at, Price}),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, 'F.RIM5', buy, OrderQty, [{account, 'A80'},{ord_type,2}]}),
    {next_state, canceled_to_market_selling, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count + 1, price = Price}};
buying(_Event, State) ->
    {next_state, buying, State}.

selling(rejected, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count}) ->
    ?D(rejected),
    {next_state, sell_cash, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count}};
selling({filled, Qty}, #state{} = State) ->
    ?D({filled, Qty}),
    {next_state, short, State};
selling({partial, Qty}, #state{} = State) ->
    ?D({partial, Qty}),
    {next_state, selling, State};
selling({repeat_down, Price},
	#state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, order_count = Order_Count, order_qty = OrderQty}) ->
    ?D({repeat_sell_at, Price}),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, 'F.RIM5', sell, OrderQty, [{account, 'A80'},{ord_type,2}]}),
    {next_state, canceled_selling, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count + 1, price = Price}};
selling({buy, Price},
	#state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, order_count = Order_Count, order_qty = OrderQty}) ->
    ?D({double_buy_at, Price, from_partial}),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, 'F.RIM5', sell, OrderQty, [{account, 'A80'},{ord_type,2}]}),
    {next_state, canceled_buying, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count + 1, price = Price}};
selling(ready_to_buy,
	#state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, order_count = Order_Count, order_qty = OrderQty}) ->
    ?D(ready_to_buy_from_partial),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, 'F.RIM5', sell, OrderQty, [{account, 'A80'},{ord_type,2}]}),
    {next_state, ready_buy, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count + 1}};
selling({market_buy, Price},
       #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, order_count = Order_Count, order_qty = OrderQty}) ->
    ?D({market_sell_at, Price}),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, 'F.RIM5', sell, OrderQty, [{account, 'A80'},{ord_type,2}]}),
    {next_state, canceled_to_market_buying, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count + 1, price = Price}};
selling(_Event, State) ->
    {next_state, selling, State}.

canceled_buying(canceled, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, price = Price}) ->
    ?D(canceled_buying_limit),
    Pos = case ets:lookup(orders, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  0
	  end,
    OrderQty = ?LIMIT - Pos,
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, 'F.RIM5', buy, OrderQty, [{account, 'A80'},{ord_type,2},{price, Price}]}),
    {next_state, buying, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count + 1, order_count = Count, order_qty = OrderQty}};
canceled_buying(_Event, State) ->
    {next_state, canceled_buying, State}.


canceled_selling(canceled, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, price = Price}) ->
    ?D(canceled_selling_limit_short),
    Pos = case ets:lookup(orders, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  0
	  end,
    OrderQty = ?LIMIT + Pos,
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, 'F.RIM5', sell, OrderQty, [{account, 'A80'},{ord_type,2},{price, Price}]}),
    {next_state, selling, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count + 1, order_count = Count, order_qty = OrderQty}};
canceled_selling(_Event, State) ->
    {next_state, canceled_selling, State}.


long({sell, Price}, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count}) ->
    ?D({sell_at, Price}),
    OrderQty = 2*?LIMIT,
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, 'F.RIM5', sell, OrderQty, [{account, 'A80'},{ord_type,2},{price, Price}]}),
    {next_state, selling,
     #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count + 1, order_count = Count, order_qty = OrderQty, price = Price}};
long(ready_to_sell, State) ->
    ?D(ready_to_sell),
    {next_state, ready_sell, State};
long({market_sell, Price}, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count}) ->
    ?D({sell_market_at, Price}),
    OrderQty = ?LIMIT,
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, 'F.RIM5', sell, OrderQty, [{account, 'A80'},{ord_type,2},{price, Price}]}),
    {next_state, sell_cash,
     #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count + 1, order_count = Count, order_qty = OrderQty, price = Price}};
long(_Event, State) ->
    {next_state, long, State}.

short({buy, Price}, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count}) ->
    ?D({buy_at, Price}),
    OrderQty = 2*?LIMIT,
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, 'F.RIM5', buy, OrderQty, [{account, 'A80'},{ord_type,2},{price, Price}]}),
    {next_state, buying,
     #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count + 1, order_count = Count, order_qty = OrderQty, price = Price}};
short(ready_to_buy, State) ->
    ?D(ready_to_buy),
    {next_state, ready_buy, State};
short({market_buy, Price}, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count}) ->
    ?D({buy_market_at, Price}),
    OrderQty = ?LIMIT,
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, 'F.RIM5', buy, OrderQty, [{account, 'A80'},{ord_type,2},{price, Price}]}),
    {next_state, buy_cash,
     #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count + 1, order_count = Count, order_qty = OrderQty, price = Price}};
short(_Event, State) ->
    {next_state, short, State}.



ready_sell({sell, Price}, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count}) ->
    ?D({sell_at, Price}),
    Pos = case ets:lookup(orders, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  empty
	  end,
    if Pos > 0 ->
	    OrderQty = Pos,
	    gen_event:notify(PidTo,{new_order_single, SendTo, Count, 'F.RIM5', sell, OrderQty,[{account, 'A80'},{ord_type,2},{price, Price}]}),
	    {next_state, selling_cash,
	     #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count + 1, order_count = Count, order_qty = OrderQty, price = Price}};
       Pos =< 0, Pos > -?LIMIT ->
	    {next_state, sell_cash, #state{pid_exec_conn = SendTo, pid_event = PidTo}};
       Pos == -?LIMIT ->
	    {next_state, short, #state{pid_exec_conn = SendTo, pid_event = PidTo}}
    end;
ready_sell(_Event, State) ->
    {next_state, ready_sell, State}.

ready_buy({buy, Price}, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count}) ->
    ?D({buy_at, Price}),
    Pos = case ets:lookup(orders, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  empty
	  end,
    if Pos < 0 ->
	    OrderQty = -Pos,
	    gen_event:notify(PidTo,{new_order_single, SendTo, Count, 'F.RIM5', buy, OrderQty,[{account, 'A80'},{ord_type,2},{price, Price}]}),
	    {next_state, buying_cash,
	     #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count + 1, order_count = Count, order_qty = OrderQty, price = Price}};
       Pos >= 0, Pos < ?LIMIT ->
	    {next_state, buy_cash, #state{pid_exec_conn = SendTo, pid_event = PidTo}};
       Pos == ?LIMIT ->
	    {next_state, long, #state{pid_exec_conn = SendTo, pid_event = PidTo}}
    end;
ready_buy(_Event, State) ->
    {next_state, ready_buy, State}.


selling_cash({filled, Qty}, #state{} = State) ->
    ?D({filled, Qty}),
    {next_state, sell_cash, State};
selling_cash({partial, Qty}, State) ->
    ?D({partial, Qty}),
    {next_state, selling_cash, State};
selling_cash({repeat_down, Price},
	     #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, order_count = Order_Count, order_qty = OrderQty}) ->
    ?D({double_sell, Price}),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, 'F.RIM5', sell, OrderQty, [{account, 'A80'},{ord_type,2}]}),
    {next_state, canceled_selling, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count + 1, price = Price}}.

buying_cash({filled, Qty}, #state{} = State) ->
    ?D({filled, Qty}),
    {next_state, buy_cash, State};
buying_cash({partial, Qty}, State) ->
    ?D({partial, Qty}),
    {next_state, buying_cash, State};
buying_cash({repeat_up, Price},
	    #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, order_count = Order_Count, order_qty = OrderQty}) ->
    ?D({double_buy, Price}),
    gen_event:notify(PidTo, {cancel_order, SendTo, Count, Order_Count, 'F.RIM5', buy, OrderQty, [{account, 'A80'},{ord_type,2}]}),
    {next_state, canceled_buying, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count + 1, price = Price}}.



sell_cash({sell, Price}, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count}) ->
    ?D({sell_at, Price}),
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, 'F.RIM5', sell, ?LIMIT, [{account, 'A80'},{ord_type,2},{price, Price}]}),
    {next_state, selling, #state{count = Count + 1, order_count = Count, order_qty = ?LIMIT, price = Price}};
sell_cash(_Event, State) ->
    {next_state, sell_cash, State}.

buy_cash({buy, Price}, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count}) ->
    ?D({buy_at, Price}),
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, 'F.RIM5', buy, ?LIMIT, [{account, 'A80'},{ord_type,2},{price, Price}]}),
    {next_state, buying, #state{count = Count + 1, order_count = Count, order_qty = ?LIMIT, price = Price}};
buy_cash(_Event, State) ->
    {next_state, buy_cash, State}.



canceled_to_market_buying(canceled, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, price = Price}) ->
    ?D(canceled_buying_limit),
    Pos = case ets:lookup(orders, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  0
	  end,
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, 'F.RIM5', buy, ?LIMIT - Pos, [{account, 'A80'},{ord_type,2},{price, Price}]}),
    {next_state, buy_market, #state{count = Count + 1, order_count = Count, order_qty = ?LIMIT - Pos}};
canceled_to_market_buying(_Event, State) ->
    {next_state, canceled, State}.


canceled_to_market_selling(canceled, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, price = Price}) ->
    ?D(canceled_selling_limit_short),
    Pos = case ets:lookup(orders, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  0
	  end,
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, 'F.RIM5', sell, ?LIMIT + Pos, [{account, 'A80'},{ord_type,2},{price, Price}]}),
    {next_state, sell_marlet, #state{count = Count + 1, order_count = Count, order_qty = ?LIMIT + Pos}};
canceled_to_market_selling(_Event, State) ->
    {next_state, canceled_selling, State}.






sell_market({market_sell, Price}, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count}) ->
    ?D({sell_at, Price}),
    Pos = case ets:lookup(orders, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  0
	  end,
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, 'F.RIM5', sell, Pos, [{account, 'A80'},{ord_type,2},{price, Price}]}),
    {next_state, sell_cash, #state{count = Count + 1, order_count = Count, order_qty = Pos, price = Price}};
sell_market(_Event, State) ->
    {next_state, sell_market, State}.

buy_market({market_buy, Price}, #state{pid_exec_conn = SendTo, pid_event = PidTo, count = Count, price = Price}) ->
    ?D({buy_at, Price}),
    Pos = case ets:lookup(orders, pos) of
	      [{_, Poss}] ->
		  Poss;
	      _ ->
		  0
	  end,
    gen_event:notify(PidTo, {new_order_single, SendTo, Count, 'F.RIM5', buy, Pos, [{account, 'A80'},{ord_type,2},{price, Price}]}),
    {next_state, buy_cash, #state{count = Count + 1, order_count = Count, order_qty = Pos, price = Price}};
buy_market(_Event, State) ->
    {next_state, buy_market, State}.


handle_event(shutdown, _StateName, State) ->
    {stop, normal, State};
handle_event(Event, StateName, State) ->
    io:format("Receives an unknown global sync event: ~p ~n", [Event]),
    {next_state, StateName, State}.

handle_sync_event(Event, _From, StateName, State) ->
    io:format("Receives an unknown global sync event: ~p ~n", [Event]),
    {reply, "wait",  StateName, State}.

connect(PidTo, SendTo, From) ->
    gen_event:notify(PidTo, {connect, SendTo, From}).

