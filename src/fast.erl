-module(fast).
-behaviour(gen_server).

-compile(export_all).

-include("../include/fast_context.hrl").

start_link(id) ->
  gen_server:start_link(?MODULE,[Id],[]).
  
root() ->
  case code:lib_dir(fast) of
    {error, _Error} -> filename:dirname(code:which(?MODULE)) ++ "/../";
    Root_ when is_list(Root_) ->
      Root_
  end.
  
init([_Id]) ->
  F = fun([], _) ->
    ok;
  (Err, Val) ->
    io:format("~p ~p~n", [Err, Val])
  end,
  {ok, Context} = create_context({file, root() ++"/spec/templates.xml"}, [], F),
  {ok, #context{} = Context}.

handle_info({udp, _Socket, _Ip, _Port, Bin}, #context{} = Context) ->
  case fast_segment:decode(Bin, Context) of
    {ok, {TemplateName, Msg, Rest, _}} ->
      io:format("~p ~p~n", [TemplateName, Msg]),
      %{noreply, Context}
      {ok, { _, _, _, Context1}} = fast_segment:decode(Rest, Context),
      handle_info({udp, _Socket, _Ip, _Port, Rest}, Context1)
  end;
handle_info( _Info, State) ->
  {noreply, State}.
  
terminate(normal, State) ->
  {noreply, State}.
