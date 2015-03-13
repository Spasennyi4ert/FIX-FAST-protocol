-module(fast_fsm).
-behaviour(gen_fsm).

-record(state, {
	  pid_exec_conn,
	  pid_event
}).

%% public API
-export([start_link/1]).

%% callbacks
-export([init/1, terminate/3, code_change/4, handle_info/3, handle_event/3, handle_sync_event/4]).

%% custom state names
-export([wait/2, sell_market/2, buy_market/2, double_buy/2, double_sell/2, buy_limit_cash/2, sell_limit_cash/2,
	buy_limit_long/2, sell_limit_short/2, ready_to_sell/1, ready_to_buy/1]).

% events
-export([ long/2, short/2, buy/2, sell/2, reverse_down/2, reverse_up/2, 
	  ready_sell/2, ready_buy/2, market_sell/2, market_buy/2, new_order_single/7]).
	  
-export([connect/2]).

start_link(SendTo) ->
    gen_fsm:start_link(?MODULE, [SendTo], []).

long( Pid, Number) ->
    gen_fsm:send_event( Pid, {long, Number}).

short( Pid, Number) ->
    gen_fsm:send_event( Pid, {short, Number}).

buy( Pid, Number) ->
    gen_fsm:send_event( Pid, {buy, Number}).

sell( Pid, Number) ->
    gen_fsm:send_event( Pid, {sell, Number}).

reverse_down( Pid, Number) ->
    gen_fsm:send_event( Pid, {double_sell, Number}).

ready_to_sell( Pid) ->
    gen_fsm:send_event( Pid, ready_to_sell).

market_sell( Pid,  Number) ->
    gen_fsm:send_event( Pid, {market_sell, Number}).

reverse_up( Pid, Number) ->
    gen_fsm:send_event( Pid, {double_buy, Number}).

ready_to_buy( Pid) ->
    gen_fsm:send_event( Pid, ready_to_buy).

market_buy( Pid,  Number) ->
    gen_fsm:send_event( Pid, {market_buy, Number}).

init([SendTo]) ->
    fast:start_task(fast_s, 2, {fast_ind, start_link, []}),
    fast:run(fast_s, [self()]),
    {ok, PidTo} = gen_event:start_link(),
    gen_event:add_handler(PidTo, fast_msg_sender, []),
    connect(PidTo, SendTo),
    {ok, wait, #state{pid_exec_conn = SendTo, pid_event = PidTo}}.


terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

handle_info(_Info, StateName, Data) ->
    {next_state, StateName, Data}.

wait({long, Price}, #state{pid_exec_conn = SendTo, pid_event = PidTo} = State) ->
    io:format("buy at: ~p~n", [Price]),
    new_order_single(PidTo, SendTo, 1, 'F.RIH5', buy, 1, [{account, 'A80'},{ord_type,2},{price, Price}]),
    {next_state, buy_limit_long, State};
wait({short, Price}, #state{pid_exec_conn = SendTo, pid_event = PidTo} = State) ->
    io:format("sell at: ~p~n", [Price]),
    %gen_event:notify(PidTo, {new_order_single(SendTo, 1, 'F.RIH5', sell, 1, [{account, 'A80'},{ord_type,2},{price, Price}])}),
    {next_state, sell_limit_short, State};
wait(_Event, State) ->
    {next_state, wait, State}.


buy_limit_long({double_sell, Price}, State) ->
    io:format("double_sell at: ~p~n", [Price]),
    {next_state, double_sell, State};
buy_limit_long(ready_to_sell, State) ->
    io:format("ready_to_sell~n"),
    {next_state, ready_sell, State};
buy_limit_long({market_sell, Price}, State) ->
    io:format("sell_market at: ~p~n", [Price]),
    {next_state, sell_market, State};
buy_limit_long(_Event, State) ->
    {next_state, buy_limit_long, State}.

sell_limit_short({double_buy, Price}, State) ->
    io:format("double_buy at: ~p~n", [Price]),
    {next_state, double_buy, State};
sell_limit_short(ready_to_buy, State) ->
    io:format("ready_to_buy~n"),
    {next_state, ready_buy, State};
sell_limit_short({market_buy, Price}, State) ->
    io:format("buy_market at: ~p~n", [Price]),
    {next_state, buy_market, State};
sell_limit_short(_Event, State) ->
    {next_state, sell_limit_short, State}.

ready_sell({sell, Price}, State) ->
    io:format("sell at: ~p~n", [Price]),
    {next_state, sell_limit_cash, State};
ready_sell({double_sell, Price}, State) ->
    io:format("double_sell at: ~p~n", [Price]),
    {next_state, double_sell, State};
ready_sell(_Event, State) ->
    {next_state, ready_sell, State}.

ready_buy({buy, Price}, State) ->
    io:format("buy at: ~p~n", [Price]),
    {next_state, buy_limit_cash, State};
ready_buy({double_buy, Price}, State) ->
    io:format("double_buy at: ~p~n", [Price]),
    {next_state, double_buy, State};
ready_buy(_Event, State) ->
    {next_state, ready_buy, State}.

double_sell({double_buy, Price}, State) ->
    io:format("double_buy at: ~p~n", [Price]),
    {next_state, double_buy, State};
double_sell(ready_to_buy, State) ->
    io:format("ready_to_buy~n"),
    {next_state, ready_buy, State};
double_sell({buy_market, Price}, State) ->
    io:format("buy_market at: ~p~n", [Price]),
    {next_state, buy_market, State};
double_sell(_Event, State) ->
    {next_state, double_sell, State}.

double_buy({double_sell, Price}, State) ->
    io:format("double_sell at: ~p~n", [Price]),
    {next_state, double_sell, State};
double_buy(ready_to_sell, State) ->
    io:format("ready_to_sell~n"),
    {next_state, ready_sell, State};
double_buy({sell_market, Price}, State) ->
    io:format("sell_market at: ~p~n", [Price]),
    {next_state, sell_market, State};
double_buy(_Event, State) ->
    {next_state, double_buy, State}.

sell_market({sell, Price}, State) ->
    io:format("sell at: ~p~n", [Price]),
    {next_state, sell_limit_short, State};
sell_market(_Event, State) ->
    {next_state, sell_market, State}.

buy_market({buy, Price}, State) ->
    io:format("buy at: ~p~n", [Price]),
    {next_state, buy_limit_long, State};
buy_market(_Event, State) ->
    {next_state, buy_market, State}.

sell_limit_cash({buy, Price}, State) ->
    io:format("buy at: ~p~n", [Price]),
    {next_state, buy_limit_long, State};
sell_limit_cash(_Event, State) ->
    {next_state, sell_limit_cash, State}.

buy_limit_cash({sell, Price}, State) ->
    io:format("sell at: ~p~n", [Price]),
    {next_state, sell_limit_long, State};
buy_limit_cash(_Event, State) ->
    {next_state, buy_limit_cash, State}.

handle_event(shutdown, _StateName, State) ->
    {stop, normal, State};
handle_event(Event, StateName, State) ->
    io:format("Receives an unknown global sync event: ~p ~n", [Event]),
    {next_state, StateName, State}.

handle_sync_event(Event, _From, StateName, State) ->
    io:format("Receives an unknown global sync event: ~p ~n", [Event]),
    {reply, "wait",  StateName, State}.

connect(PidTo, SendTo) ->
    gen_event:notify(PidTo, {connect, SendTo}).

new_order_single(_PidTo, SendTo, 1, 'F.RIH5', buy, 1, [{account, 'A80'},{ord_type,2},{price, Price}]) ->
   fast_msg_sender:new_order_single(SendTo, 1, 'F.RIH5', buy, 1, [{account, 'A80'},{ord_type,2},{price, Price}]).
   %  gen_event:notify(PidTo, {new_order_single(SendTo, 1, 'F.RIH5', buy, 1, [{account, 'A80'},{ord_type,2},{price, Price}])}).
