%%%----------------------------------------------------------------------
%%%
%%% @copyright litaocheng
%%%
%%% @author litaocheng@gmail.com
%%% @doc logging header file
%%%
%%%----------------------------------------------------------------------
-ifndef(LOGGING_HRL).
-define(LOGGING_HRL, ok).
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(log_record, {
        time,                   % the timestamp for log
        name,                   % the logger name
        level,                  % the log level
        module,                 % the module
        lineno,                 % the line number
        pid,                    % the erlang process id
        ospid,                  % the process id in os
        message                 % the message body
    }).
-type log_record() :: #log_record{}.

-type log_name() :: string() | atom().
-type log_level() :: non_neg_integer().
-type log_handler() :: {atom(), term()}.
-type log_formatter() :: string().


-endif. % LOGGING_HRL
