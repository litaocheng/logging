%%%----------------------------------------------------------------------
%%%
%%% @copyright litaocheng
%%%
%%% @author litaocheng <litaocheng@gmail.com>
%%% @doc log handler behaviour
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(logging_handler).
-author("litaocheng@gmail.com").
-vsn('0.1').
-include("logging.hrl").

-export([behaviour_info/1]).
-export([not_use/0]).

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].
behaviour_info(callbacks) ->
    [{init,1},
    {log,3},
    {terminate,2}];
behaviour_info(_Other) ->
    undefined.

not_use() ->
    init(ok),
    log(a, b, c),
    terminate(a, b).


%%
%% the below is the handler code example
%% 

%% @doc init callback
-spec init(any()) ->
    {'ok', any()} | {'ok', any(), hibernate} | any().
init(_Args) ->
    {ok, []}.

%% @doc handle the log
-spec log(log_record(), iolist(), any()) ->
    {'ok', any()} | {'ok', any(), hibernate} | 'remove_handler' | any().
log(_LogRecord, _LogBin, State) ->
    {ok, State}.

%% @doc terminate the log handler, the return of this function is 
%% any term, if the log handler is deleted by the delete_handler, then
%% the return value of this function will be the return value of delete_handler;
%% otherwise the return value is ignore
-spec terminate(any(), any()) ->
    any().
terminate(_Reason, _State) ->
    ok.
