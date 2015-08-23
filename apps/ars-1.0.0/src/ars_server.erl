-module(ars_server).
-behavior(gen_server).

-define(SPEC(MFA),
	{worker_sup,
	 {ars_task_sup, start_link, [MFA]},
	 temporary, 1000, supervisor, [ars_task_sup]}).

-record(state, {sup,
		refs,
		name
}).

-export([start/3, start_link/3, run/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).


start(Name, Sup, MFA) when is_atom(Name) ->
    gen_server:start({local, Name}, ?MODULE, {Name, MFA, Sup}, []).

start_link(Name, Sup, MFA) when is_atom(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, {Name, MFA, Sup}, []).

run(Name, Args) ->
    gen_server:call(Name, {run, Args}).

stop(Name) ->
    gen_server:call(Name, stop).

init({Name, MFA, Sup}) ->
    self() ! {start_worker_supervisor, Sup, MFA},
    {ok, #state{refs = gb_trees:empty(), name = Name}}.

handle_call({run, Args}, _From, S = #state{sup = Sup, refs = R, name = Name}) ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {reply, {ok, Pid}, S#state{refs = gb_trees:insert(Name, {Pid, Ref, Args}, R)}};
handle_call(stop, _From, S = #state{refs = Refs, name = Name}) ->
    {Pid, _Ref, _Args} = gb_trees:get(Name, Refs),
    case id(Name) of
	ars_fsm ->
	    ars_fsm:close(Pid),
	    {reply, {ok, Pid}, S};
	_ ->
	    {noreply, S}
    end;
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({'DOWN', _Ref, process, _Pid, _}, S) ->
    {stop, {shutdown, worker_dead}, S};
handle_info({start_worker_supervisor, Sup, MFA}, S = #state{}) ->
    {ok, Pid} = supervisor:start_child(Sup, ?SPEC(MFA)),
    link(Pid),
    {noreply, S#state{sup = Pid}};
handle_info(Msg, State) ->
    io:format("Unknown message: ~p~n", Msg),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(normal, #state{}) ->
    ok;
terminate(_Reason, _State) ->
    ok.

id(Name) ->
    list_to_atom("ars_" ++ atom_to_list(Name)).
