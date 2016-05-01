
-module(fast_dispatcher).
-behaviour(gen_server).

-record(state, {
	  item,
	  table,
	  seq = 0,
	  count = 1,
	  count_dup = 1
}).

-define(SERVER, global:whereis_name(?MODULE)).

-export([start_link/2, feed/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2, init/1]).

start_link(Type, Item) ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [Type, Item], []).

feed(Msg) ->
    gen_server:cast(?SERVER, {feed, Msg}).

init([Type, Item]) ->
    Tid = ets:new(Type, [ordered_set, public]),
    {ok,  #state{item = Item, table = Tid}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({feed, {Act, Type, SecID, Rpt, Exp, Mant}}, S = #state{item = Item}) ->
    case SecID of
	Item ->
	    processed({Rpt, Act, Type, Exp, Mant}, S);
	_ ->
	    {noreply, S}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_, #state{table = Tid}) ->
    ets:delete(Tid),
    ok.

processed({Rpt, Act, Type, Exp, Mant}, S = #state{table = Tid, seq = Seq, count = Count, count_dup = CD}) ->
    case ets:lookup(Tid, Rpt) of
	[] ->
	    if Rpt == Count andalso Act == 0 andalso Type == 50 ->
		    %io:format("Msg : ~p~n", [{Rpt, Exp, Mant}]),
		    ets:insert(Tid, {Rpt, Exp, Mant}),
		    price(Exp, Mant),
		    ets:delete(Tid, Seq),
		    {noreply,S#state{seq = Rpt, count = Rpt + 1}};
	       Rpt == Count ->
		    {noreply,S#state{count = Rpt + 1}};
	       Rpt > Count ->
		    if Rpt == CD ->
			    {noreply,S#state{count = Rpt + 1, count_dup = CD + 1}};
		       Rpt > CD ->
			    {noreply,S#state{count_dup = Rpt + 1}};
		       true ->
			    {noreply, S}
		    end;
	       true ->
		    {noreply, S}
	    end;
	[_] ->
	    {noreply, S}
    end.

price(Exp, Mant) ->
    Price = case Exp of
		123 ->
		    float(round(Mant * math:pow(10, -5)));
		_ ->
		    ok
	    end,
    ars_handler:get_data(Price),
    io:format("Price: ~p~n", [Price]).
