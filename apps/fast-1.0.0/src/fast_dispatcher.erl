-module(fast_dispatcher).
-behaviour(gen_server).

-record(state, {
	  table,
	  seq = 0,
	  count = 1,
	  count_dup = 1
}).

-export([start_link/0, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2, init/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->

    Tid = ets:new(data,[ordered_set, public]),
    {ok,  #state{ table = Tid}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_, #state{table = Tid}) ->
    ets:delete(Tid),
    ok.
