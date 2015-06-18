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
	seq
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

handle_cast({feed, {SeqRpt, Expo, Mant}}, #state{table = Tid, seq = SeqNum} = State) ->
	case ets:lookup(Tid,SeqRpt) of
	[] ->
	    ets:insert(Tid, {SeqRpt,  Expo, Mant}),
	    Last = ets:last(Tid),
	    [{Se, Expo, Mantissa}] = ets:lookup(Tid, Last),
	    price(Se, Expo, Mantissa),
	    ets:delete(Tid,SeqNum),
	    {noreply, #state{table = Tid, seq = SeqRpt - 5}};
	[{_, _, _}] ->
	    ok,
	    {noreply, State};
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

terminate(_, _) ->
	ftrc:stop(),
    ok.

price(Se, Expo, Mantissa) ->
	Exponenta = Expo - 1,
    Price = Mantissa * math:pow(10, Exponenta),
	?D ({Se, Price}).
    %fast_ind:get_data(Price).
