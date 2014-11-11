-module(fast_in).
-behaviour(gen_server).

-compile(export_all).

-record(state, {
	store
}).

start_link() ->
	gen_server:start_link(?MODULE, [], []).

get_data(Number) ->
	gen_server:cast(?MODULE, {get_data, Number}).

init([]) ->
	%{ok, #state{store = ets:new(store, [named_table, set, public])}}.
	{ok, dict:new()}.
%get_data(Number) ->
	%ets:new(store, [named_table, set, public]),
%	io:format("Number: ~p ~n ~n", [Number]).

handle_cast({get_data, Number}, State) ->
	
    	NewState = dict:store(store, Number, State),
	%ets:new(store, [named_table, set, public]),
	io:format("Number: ~p ~n ~n", [Number]),
	Response = {ok, Number},
	{reply,Response,NewState}.

handle_info(_Info, State) ->
	{noreply, State}.
terminate(_Reason, _State) ->
	ok.
