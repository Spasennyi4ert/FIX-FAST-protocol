-module(fast_fsm_decode).
-behaviour(gen_fsm).

-compile(export_all).

-include("../include/fast_server.hrl").

-record(state, {
	  pid_server,
	  seq = 0
}).

%% public API
-export([start_link/1]).

%% callbacks
-export([init/1, terminate/3, code_change/4, handle_info/3, handle_event/3, handle_sync_event/4]).

% events
-export([feed/2]).

%% custom state names
-export([feed_a/2, feed_b/2]).

% privat


start_link(SendTo) ->
    gen_fsm:start_link(?MODULE, [SendTo], []).

feed(Pid, {PidTo, Name, Bin}) ->
    gen_fsm:send_event(Pid, {feed, PidTo, Name, Bin}).

init(SendTo) ->
	fast_server:connect(SendTo, self()),
	{ok, feed_a, #state{pid_server = SendTo}}.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

handle_info(_Info, StateName, Data) ->
    {next_state, StateName, Data}.

feed_a({feed, _PidTo, Name, << _Head:32, MsgSeqNum:16/bits, Bin/bytes>>}, #state{seq = SeqNum}) ->
    case Name of
	feed_a ->
	    {Seq, _L, _Rest} = fast_decode:decode_type(MsgSeqNum),
	    case Seq of
		SeqNum ->
		    io:format("~p ~p~n ~n", [Name ,SeqNum]),
		    fast_server:feed({Name, Bin}),
		    {next_state, feed_a, #state{seq = SeqNum + 1}};
		_ ->
		    {next_state, feed_b, #state{seq = Seq + 1}}
	    end;
	_ ->
	    ok
    end;
feed_a(_Event, State) ->
    {next_state, feed_a, State}.

feed_b({feed, _PidTo, Name, << _Head:32, MsgSeqNum:16/bits, Bin/bytes>>}, #state{seq = SeqNum}) ->
    case Name of
	feed_b ->
	    {Seq, _L, _Rest} = fast_decode:decode_type(MsgSeqNum),
	    case Seq of
		SeqNum ->
		    io:format("~p ~p~n ~n", [Name ,SeqNum]),
		    fast_server:feed({Name, Bin}),
		    {next_state, feed_b, #state{seq = SeqNum + 1}};
		_ ->
		    {next_state, feed_a, #state{seq = Seq + 1}}
	    end;
	_ ->
	    ok
    end;
feed_b(_Event, State) ->
    {next_state, feed_b, State}.



handle_event(shutdown, _StateName, State) ->
    {stop, normal, State};
handle_event(Event, StateName, State) ->
    io:format("Receives an unknown global sync event: ~p ~n", [Event]),
    {next_state, StateName, State}.

handle_sync_event(Event, _From, StateName, State) ->
    io:format("Receives an unknown global sync event: ~p ~n", [Event]),
    {reply, "wait",  StateName, State}.

%connect( SendTo, From) ->
 %   fast_server:connect( SendTo, From).
