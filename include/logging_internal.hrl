%%%----------------------------------------------------------------------
%%%
%%% @copyright litaocheng
%%%
%%% @author litaocheng@gmail.com
%%% @doc logging internal header file
%%%
%%%----------------------------------------------------------------------
-ifndef(LOGGING_INTERNAL_HRL).
-define(LOGGING_INTERNAL_HRL, ok).

-define(PREFIX, 'logger_').

-define(LOGLEVEL_STR, <<"loglevel ">>).

-define(FORMATTER_DEFAULT, "(asctime) - (name) (levelname) - (message)").

%%      Level   Numeric value
-define(CRITICAL,    50).
-define(ERROR,       40).
-define(WARNING,     30).
-define(INFO,        20).
-define(DEBUG,       10).
-define(NOTSET,      0).

%% log handler
-record(handler, {
        module             :: atom(),
        state}).
-type handler() :: #handler{}.


-endif. % LOGGING_INTERNAL_HRL
