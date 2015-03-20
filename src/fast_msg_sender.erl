-module(fast_msg_sender).
-behavior(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
	 code_change/3, terminate/2]).

init([]) ->
    {ok, []}.

handle_event({connect, Pid}, State) ->
    fast_exec_conn:connect(Pid),
    {ok, State};
handle_event({new_order_single, Pid, OrderId, Stock, Side, Quantaty, Options}, State) ->
    fast_exec_conn:new_order_single(Pid, OrderId, Stock, Side, Quantaty, Options),
    {ok, State};
handle_event({cancel_order, Pid, OrderId, CancOrderId, Stock, Side, Quantaty, Options}, State) ->
    fast_exec_conn:cancel_order(Pid, OrderId, CancOrderId, Stock, Side, Quantaty, Options),
    {ok, State};
handle_event(_, State) ->
    {ok, State}.

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
