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
  {ok, Context} = create_context({file, root() ++"/templates.xml"}, [], F),
  {ok, #context{} = Context}.
