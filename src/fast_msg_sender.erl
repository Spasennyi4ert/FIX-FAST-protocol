-module(fast_msg_sender).
-behavior(gen_event).
-include("log.hrl").

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
	 code_change/3, terminate/2]).

init([]) ->
    {ok, []}.

handle_event({connect, Pid, From}, State) ->
    fast_exec_conn:connect(Pid, From),
    {ok, State};
handle_event({new_order_single, Pid, OrderId, Side, Quantaty, Options}, State) ->
    fast_exec_conn:new_order_single(Pid, OrderId, Side, Quantaty, Options),
    ?D(new_order_single),
    {ok, State};
handle_event({cancel_order, Pid, OrderId, CancOrdId, Side, Quantaty, Options}, State) ->
    fast_exec_conn:cancel_order(Pid, OrderId, CancOrdId, Side, Quantaty, Options),
    ?D(cancel_order),
    {ok, State};
handle_event(_, State) ->
    ?D(State),
    {ok, State}.

handle_call(_, State) ->
    ?D(State),
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(close, State) ->
    ?D(State),
    ok.
