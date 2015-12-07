-module(ars_mng).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2]).

-record(state, {
	  name,
	  table_id
}).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

init([Name]) ->
    process_flag(trap_exit, true),
    self() ! start,
    {ok, #state{name = Name}}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({prime, Data}, #state{name = Name} = S) ->
    FSM = whereis(ars_fsm),
    link(FSM),
    TableId = ets:new(Name, [private]),
    ets:insert(TableId, Data),
    ets:setopts(TableId, {heir, self(), Data}),
    ets:give_away(TableId, FSM, Data),
    {noreply, S#state{table_id = TableId}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start, State) ->
    gen_server:cast(self(), {prime, {pos, 0}}),
    {noreply, State};
handle_info({'EXIT', Pid, killed}, #state{table_id = TableId} = S) ->
    io:format("FSM(~p) !! now dead, farewell TableID: ~p~n", [Pid, TableId]),
    {noreply, S};
handle_info({'ETS-TRANSFER', TableId, Pid, Data}, S) ->
    FSM = wait_for_fsm(),
    io:format("Warning TableId: ~p OwnerPid: ~p is dying~n"
	      "FSM(~p) => MGR(~p) handling TableId: ~p~n", [TableId, Pid, Pid, self(), TableId]),
    link(FSM),
    ets:give_away(TableId, FSM, Data),
    {noreply, S#state{table_id = TableId}};
handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

wait_for_fsm() ->
    case whereis(ars_fsm) of
	undefined ->
	    timer:sleep(1),
	    wait_for_fsm();
	Pid ->
	    Pid
    end.
