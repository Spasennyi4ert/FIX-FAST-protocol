-module(fast_server).
-behaviour(gen_server).

-compile(export_all).

-include("../include/fast_context.hrl").

-record(state, {
	socket,
	context
}).
-record(server, {
	buffer
}).


start_link() ->
	gen_server:start_link(?MODULE, [], []).

root() ->
	case code:lib_dir(fast) of
		{error, _Error} ->
			filename:dirname(code:which(?MODULE)) ++ "/../";
			Root_ when is_list(Root_) ->
				Root_
	end.

init([]) ->
        ftrc:start(),
	
	Source = "172.27.129.77",
	Port = "24027",
	Group = "239.192.7.27",

	{ok,SourceAddress} = inet:parse_address(Source),
	{ok,GroupAddress} = inet:parse_address(Group),
		GroupIp = ip_to_binary(GroupAddress),
		LocalIp = ip_to_binary({0,0,0,0}),
		SourceIp = ip_to_binary(SourceAddress),
		Bin = << GroupIp/binary,LocalIp/binary,SourceIp/binary >>,

	{ok, Socket} = gen_udp:open(erlang:list_to_integer(Port),
      [
         inet,
         binary,
         {active, true},
         {reuseaddr, true},
	{multicast_ttl, 30},
         {raw, 0, 39, Bin}
      ]),
    ok = gen_udp:controlling_process(Socket, self()),  %% 35
    F = fun([], _) ->
    	ok;
    (Err, Val) ->
    	io:format("~p ~p~n", [Err, Val])
    	end,
    try
    	{Dicts, Templates} = fast_xml:parse({file, root() ++ "/spec/templates.xml"}, []),
	{ok, #context{dicts = Dicts, templates = Templates, logger = F, options = [], socket = Socket}}
    catch
	_:Err ->
		Err
    end.

ip_to_binary(Ip) ->
	list_to_binary(tuple_to_list(Ip)).

handle_info({udp, _Socket, _Ip, _Port, Bin}, #context{} = Context) ->
	case fast_segment:decode(Bin, Context) of
		{ok, {TemplateName, Msg, Rest, _}} ->
			io:format("~p ~p~n", [TemplateName, Msg]),
			{ok, { _, _, _, Context1}} = fast_segment:decode(Rest, Context),
			handle_info({udp, _Socket, _Ip, _Port, Rest}, Context1);
		{error, Reason} ->
			io:format("ERROR: ~p, Bin = ~p ~n", [Reason, Bin]),
			exit(failed)
	end;
handle_info(_Info, State) ->
	{noreply, State}.

terminate(normal, State) ->
        ftrc:stop(),
	{noreply, State}.

decode(Bin, Context) ->
    decode(Bin, Context, 0).

%% I - just a counter for debugging
decode(<<>>, Context, I) ->
    io:format("moar, ~p~n", [I]),
    Context;
decode(Bin, Context, I) ->
    case fast_segment:decode(Bin, Context) of
        {ok, {TemplateName, Msg, Rest, Ctx2}} ->
            io:format("~p ~p~n", [TemplateName, Msg]),
            decode(Rest, Ctx2, I + 1);
        {error, Reason} ->
            io:format("ERROR: ~p, Bin = ~p ~n", [Reason, Bin]),
            exit(failed)
    end.
