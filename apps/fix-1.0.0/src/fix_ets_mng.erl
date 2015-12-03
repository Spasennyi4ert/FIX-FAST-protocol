-module(fix_ets_mng).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-include("log.hrl").

-record(state, {
	  names,
	  table_id
}).

start_link(Name) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Name], []).

init([Name]) ->
    process_flag(trap_exit, true),
    self() ! start,
    {ok, #state{names = Name}}.

handle_call(_Msg, _From, S) ->
    {reply, {error, undef}, S}.


handle_cast({prime, Data}, #state{names = Name} = S) ->
    FEC = whereis(fix_exec_conn),
    link(FEC),
    TableId = ets:new(Name, [private]),
    ets:insert(TableId, Data),
    ets:setopts(TableId, {heir, self(), Data}),
    ets:give_away(TableId, FEC, Data),
    fix_exec_conn:connect(FEC),
    {noreply, S#state{table_id = TableId}};
handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info(start, S) ->
    gen_server:cast(self(), {prime, {seq, 1}}),
    {noreply, S};
handle_info({'EXIT', Pid, killed}, #state{table_id = TableId} = S) ->
    io:format("FEC(~p) !! is now dead, farewell TableId: ~p~n", [Pid, TableId]),
    {noreply, S};
handle_info({'ETS-TRANSFER', TableId, Pid, Data}, S) ->
    FEC = wait_for_fec(),
    io:format("Warning TableId: ~p OwnerPid: ~p is diyng~n"
	      "FEC(~p) => MGR(~p) handling TableId: ~p~n", [TableId, Pid, Pid, self(), TableId]),
    link(FEC),
    ets:give_away(TableId, FEC, Data),
    {noreply, S#state{table_id = TableId}};
handle_info(_Msg, S) ->
    {noreply, S}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _S) ->
    ok.

wait_for_fec() ->
    case whereis(fix_exec_conn) of
	undefined ->
	    timer:sleep(1),
	    wait_for_fec();
	Pid ->
	    Pid
    end.
