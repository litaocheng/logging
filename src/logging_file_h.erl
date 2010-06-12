%%%----------------------------------------------------------------------
%%%
%%% @copyright litaocheng
%%%
%%% @author litaocheng <litaocheng@gmail.com>
%%% @doc a handler that writes all log to stdout
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(logging_file_h).
-author("litaocheng@gmail.com").
-vsn('0.1').
-behaviour(logging_handler).

-export([init/1, log/3, terminate/2]).

init(File) ->
    case file:open(File, [raw, write, append]) of
        {ok, Fd} ->
            {ok, {File, Fd}};
        What ->
            What
    end.
    
log(LogRecord, LogBin, State = {File, Fd}) ->
    Reply = file:write(Fd, LogBin),
    {Reply, State}.

terminate(_Reason, {File, Fd}) ->
    file:sync(Fd),
    file:close(Fd).
