# Overview
logging is an erlang library, which provides the log facility. it is inspired by python logging lib.
using loggging, you can control your log output style(Handler), define log entry format(Formater).
you can also develop the log handler for yourself.

# Usage
the below is the usage code segments
First of all, let us define some macro to simplify our code.
    -define(DEBUG(F, D), 
        Logger:debug(?MODULE, ?LINE, io_lib:format(F, D))).
    -define(INFO(F, D), 
        Logger:info(?MODULE, ?LINE, io_lib:format(F, D))).
    -define(WARNING(F, D), 
        Logger:warning(?MODULE, ?LINE, io_lib:format(F, D))).
    -define(ERROR(F, D), 
        Logger:error(?MODULE, ?LINE, io_lib:format(F, D))).
    -define(CRITICAL(F, D), 
        Logger:critical(?MODULE, ?LINE, io_lib:format(F, D))).
    -define(LOGGING(Level, F, D), 
        Logger:log(Level, ?MODULE, ?LINE, io_lib:format(F, D))).

## tty handler
    % the default handler is logging_handler_tty, the default level is WARNING
    {ok, _} = logging:start("tty"),
    Logger = logging:get_logger("tty"),
    ok = Logger:set_formatter("(universal) - (name) -(module):(lineno) - (levelname) - (message)"),
    ?DEBUG("logging 1", []),
    ?INFO("logging 1", []),
    ?WARNING("logging 1", []),
    ?ERROR("logging 1", []),
    ?LOGGING(25, "logging 1", []),
    ?LOGGING(35, "logging 1", []),
    ?LOGGING(200, "logging 1", []),
    logging:stop(Logger:logger_name()),
    ok.

## file handler
    Formatter = "(universal) - (name) - (levelno) (levelname) - (message)\n",
    {ok, _} = logging:start("test_file", ?LEVEL_NOTSET, {logging_handler_file, "test_file.log"}, Formatter),
    Logger = logging:get_logger("test_file"),

    ?DEBUG("logging 1", []),
    ?INFO("logging 1", []),
    ?WARNING("logging 1", []),
    ?ERROR("logging 1", []),
    ?CRITICAL("logging 1", []),
    ?LOGGING(100, "logging 1", []),
    logging:stop(Logger:logger_name()),
    ok.

## rotate handler
    Formatter = "(name) - (levelno) (levelname) - (message)\n",
    Handler = {logging_handler_rotate, {"/tmp/test_rotate_1024_10", "my.log", 1024, 10}},
    {ok, _} = logging:start("test_rotate_base", ?LEVEL_NOTSET, Handler, Formatter),
    Logger = logging:get_logger("test_rotate_base"),
    [begin
        ?DEBUG("logging ~b", [N]),
        ?INFO("logging ~b", [N]),
        ?WARNING("logging ~b", [N]),
        ?ERROR("logging ~b", [N]),
        ?CRITICAL("logging ~b", [N]),
        ?LOGGING(100, "logging ~b", [N])
    end || N <- lists:seq(1, 100)],
    logging:stop(Logger:logger_name()),
    ok.

more information, please see the test/logging_SUITE.erl file.
