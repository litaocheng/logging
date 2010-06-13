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

-define(LOGLEVEL_STR, "LEVEL ").

-define(FORMATTER_DEFAULT, "(universal) - (name) (levelname) - (message)\n").

%%      Level   Numeric value
-define(LEVEL_CRITICAL,    50).
-define(LEVEL_ERROR,       40).
-define(LEVEL_WARNING,     30).
-define(LEVEL_INFO,        20).
-define(LEVEL_DEBUG,       10).
-define(LEVEL_NOTSET,      0).

%% log handler
-record(handler, {
        module             :: atom(),
        state}).
-type handler() :: #handler{}.


-endif. % LOGGING_INTERNAL_HRL
