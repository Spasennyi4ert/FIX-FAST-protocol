-module(fast_in).
-behaviour(gen_server).

-compile(export_all).

-record(state, {
	store
}).

start_link() ->
	gen_server:start_link({local,?MODULE},?MODULE, [], []).

stop(Pid) ->
	gen_server:call(Pid, terminate).

get_data(Number) ->
	gen_server:cast(?MODULE, {get_data, Number}).

init([]) ->
	ets:new(store, [named_table, set, public]),
	{ok, #state{}}.

handle_cast({get_data, Number}, #state{}) ->
	NewState = #state{store = Number},
	{noreply,NewState}.

handle_info(_Info, State) ->
	{noreply, State}.
terminate(_Reason, _State) ->
	ok.
