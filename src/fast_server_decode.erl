-module(fast_server_decode).
-behaviour(gen_server).

-compile(export_all).
-include_lib("stdlib/include/ms_transform.hrl").
-include("../include/fast_context.hrl").
-include("../include/fast_server.hrl").
-include("log.hrl").

-define(SECID, 10).
-define(SEQ, 7).

-record(state, {
	  table,
	  seq = 0,
	  count = 1,
	  count_dup = 1
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

feed(T) ->
    gen_server:cast(?MODULE, {feed, T}).

init([]) ->
    ftrc:start(),
    Tid = ets:new(data,[ordered_set, public]),
    {ok,  #state{ table = Tid}}.

handle_call(stop, _, P) ->
    {stop, shutdown, stopped, P};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({feed, {SeqRpt, Expo, Mant}}, #state{table = Tid, seq = SeqNum, count = Count, count_dup = CD} = State) ->
    case ets:lookup(Tid,SeqRpt) of
	[] ->
	    if SeqRpt == Count ->
		    ets_insert(Tid, {SeqRpt, Expo, Mant}),
		    Last = ets_last(Tid),
		    [{Se, Expo, Mantissa}] = ets_lookup(Tid, Last),
		    price(Se, Expo, Mantissa),
		    ets_delete(Tid,SeqNum),
		    {noreply, #state{table = Tid, seq = SeqRpt - 1, count = SeqRpt + 1, count_dup = CD}};
	       SeqRpt > Count ->
		    if SeqRpt == CD ->
			    {noreply, #state{table = Tid, seq = SeqNum, count = SeqRpt + 1, count_dup = CD + 1}};
		       SeqRpt >= CD ->
			    {noreply, #state{table = Tid, seq = SeqNum, count = SeqRpt, count_dup = CD + 1}};
		       true ->
			    ok,
			    {noreply, State}
		    end;
	       true ->
		    ok,
		    {noreply, State}
	    end;
	_ ->
	    ok,
	    {noreply, State}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_, #state{table = Tid}) ->
    ets:delete(Tid),
    ftrc:stop(),
    ok.

ets_insert(Tid, {SeqRpt,  Expo, Mant}) ->
    ets:insert(Tid, {SeqRpt,  Expo, Mant}).

ets_last(Tid) ->
    ets:last(Tid).

ets_lookup(Tid, Last) ->
    ets:lookup(Tid, Last).

ets_delete(Tid,SeqNum) ->
    ets:delete(Tid,SeqNum).

price(Se, Expo, Mantissa) ->
    Exponenta = Expo - 1,
    Price = Mantissa * math:pow(10, Exponenta),
    ?D ({Se, Price}),
    fast_ind:get_data(Price).
