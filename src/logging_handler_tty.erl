%%%----------------------------------------------------------------------
%%%
%%% @copyright litaocheng
%%%
%%% @author litaocheng <litaocheng@gmail.com>
%%% @doc a handler that writes all log to stdout
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(logging_handler_tty).
-author("litaocheng@gmail.com").
-vsn('0.1').
-behaviour(logging_handler).

-export([init/1, log/3, terminate/2]).

init(Args) ->
    % should link to user (or group_leader???)
    {ok, Args}.
    
log(_LogRecord, LogBin, State) ->
    io:format(standard_io, LogBin, []),
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
