-module(fast_task_server).
-behavior(gen_server).

-define(SPEC(MFA),
	{worker_sup,
	 {fast_task_sup, start_link, [MFA]},
	 temporary, 1000, supervisor, [fast_task_sup]}).

-record(state, {limit = 0,
		sup,
		refs,
		queue = queue:new()}).

-export([start/4, start_link/4, run/2, stop/1]).
-export([init/1, handle_call/3, handle_info/2, code_change/3, terminate/2]).

start(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
    gen_server:start({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

start_link(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->
    gen_server:start_link({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).

run(Name, Args) ->
    gen_server:call(Name, {run, Args}).

stop(Name) ->
    gen_server:call(Name, stop).

init({Limit, MFA, Sup}) ->
    self() ! {start_worker_supervisor, Sup, MFA},
    {ok, #state{limit = Limit, refs = gb_sets:empty()}}.

handle_info({'DOWN', Ref, process, _Pid, _}, S) ->
    {stop, {shutdown, worker_dead}, S};
handle_info({start_worker_supervisor, Sup, MFA}, S = #state{}) ->
    {ok, Pid} = supervisor:start_child(Sup, ?SPEC(MFA)),
    link(Pid),
    {noreply, S#state{sup = Pid}};
handle_info(Msg, State) ->
    io:format("Unknown message: ~p~n", Msg),
    {noreply, State}.

handle_call({run, Args}, _From, S = #state{limit = N, sup = Sup, refs = R}) when N > 0 ->
    {ok, Pid} = supervisor:start_child(Sup, Args),
    Ref = erlang:monitor(process, Pid),
    {reply, {ok, Pid}, S#state{limit = N-1, refs = gb_sets:add(Ref, R)}};
handle_call({run, _Args}, _From, S = #state{limit = N}) when N =< 0 ->
    {reply, noalloc, S};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
