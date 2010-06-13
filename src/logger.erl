%%%----------------------------------------------------------------------
%%%
%%% @copyright litaocheng
%%%
%%% @author litaocheng <litaocheng@gmail.com>
%%% @doc logger module
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(logger, [LoggerName]).
-author("litaocheng@gmail.com").
-vsn('0.1').
-include("logging.hrl").
-include("logging_internal.hrl").

%% logging interfaces
-export([debug/3, info/3, warning/3, error/3, critical/3, log/4]).
%% about level
-export([get_level/0, set_level/1, add_level_name/2, get_level_name/1]).
%% about handler
-export([set_handler/2, add_handler/2, delete_handler/2, all_handlers/0]).
-export([tty/0, logfile/1, rotate/4]).
%% about formatter
-export([set_formatter/1, get_formatter/0]).
-export([logger_name/0]).

debug(Mod, Line, Msg) ->
    catch do_log(?LEVEL_DEBUG, Mod, Line, Msg).

info(Mod, Line, Msg) ->
    catch do_log(?LEVEL_INFO, Mod, Line, Msg).

warning(Mod, Line, Msg) ->
    catch do_log(?LEVEL_WARNING, Mod, Line, Msg).

error(Mod, Line, Msg) ->
    catch do_log(?LEVEL_ERROR, Mod, Line, Msg).

critical(Mod, Line, Msg) ->
    catch do_log(?LEVEL_CRITICAL, Mod, Line, Msg).

log(Level, Mod, Line, Msg)  ->
    catch do_log(Level, Mod, Line, Msg).

get_level() ->
    call({get_level}).

set_level(Level) ->
    call({set_level, Level}).

%% @doc add level with the specified name, if the Level is already exists,
%% update the level name
add_level_name(Level, LevelName) ->
    call({add_level_name, Level, LevelName}).

%% @doc get the level name
get_level_name(Level) ->
    call({get_level_name, Level}).

%% @doc set log handler, the handler can be predefined handlers or
%%      user defined handler.
%%      the predefined handlers please see : logging:predef_handlers()
set_handler(Mod, Args) ->
    call({set_handler, Mod, Args}).

%% @doc add a handler to the handler list
add_handler(Mod, Args) ->
    call({add_handler, Mod, Args}).

%% @doc delete a handler from logger handler list
delete_handler(Mod, Args) ->
    call({delete_handler, Mod, Args}).

%% @doc return all the handlers from the logger
all_handlers() ->
    call({all_handlers}).

%% @doc set the handler to tty
tty() ->
    set_handler(logging_handler_tty, []).

%% @doc set the handler to file mode
logfile(File) when is_list(File) ->
    set_handler(logging_handler_file, File).

%% @doc set the handler to rotate mode 
rotate(Dir, FileName, MaxSize, MaxFile) when is_integer(MaxSize), is_integer(MaxFile),
                    MaxSize >= 1024, MaxFile > 0,
                    is_list(Dir), is_list(FileName)  ->
    set_handler(logging_handler_rotate, {Dir, FileName, MaxSize, MaxFile}).

%% @doc set the formmater for logger
set_formatter(Form) ->
    call({set_formatter, Form}).

%% @doc get the formatter string for the logger
get_formatter() ->
    call({get_formatter}).

%% @doc return the logger name
logger_name() ->
    LoggerName.

    
%%------------------------------------------------------------------------------
%%
%% internal API
%%
%%------------------------------------------------------------------------------

do_log(Level, Mod, Line, Msg) when is_integer(Level), Level >= 0 ->
    logging:valid_level(Level),
    logging:valid_module(Mod),
    logging:valid_line(Line),
    logging:valid_message(Msg),
    gen_server:cast(LoggerName, {log, group_leader(), self(), Level, Mod, Line, Msg}).


call(Req) ->
    gen_server:call(LoggerName, Req).
