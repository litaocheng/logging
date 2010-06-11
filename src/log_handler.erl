%%%----------------------------------------------------------------------
%%%
%%% @copyright litaocheng
%%%
%%% @author litaocheng <litaocheng@gmail.com>
%%% @doc log handler behaviour
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(log_handler).
-author("litaocheng@gmail.com").
-vsn('0.1').

-export([behaviour_info/1]).

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].
behaviour_info(callbacks) ->
    [{init,1},
    {log,3},
    {terminate,2}];
behaviour_info(_Other) ->
    undefined.
