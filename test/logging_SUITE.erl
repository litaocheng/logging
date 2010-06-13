-module(logging_SUITE).
%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("logging_internal.hrl").
-define(PF(F), 
    fun() ->
        V = F,
        %?INFO2("~n~80..-s\ncall\t:\t~s~nresult\t:\t~p~n~80..=s~n~n", ["-", ??F, V, "="]),
        ct:log(default, "~n~80..-s\ncall\t:\t~s~nresult\t:\t~p~n~80..=s~n~n", ["-", ??F, V, "="]),
        V
    end()).

suite() -> [
    {timetrap,{minutes,60}}
    ].  

init_per_suite(Config) ->
    code:add_path("../ebin"),
    os:cmd("rm -rf /tmp/test_rotate*"),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Name, Config) ->
    Config.

end_per_testcase(_Name, Config) ->
    {save_config, Config}.

all() -> 
    [
        test_tty,
        test_formatter,
        test_level,
        test_file,
        test_rotate_base,
        test_rotate_more,
        test_dummy
    ].

%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------
test_dummy(_Config) ->
    ok.

test_start(_Config) ->
    ok.

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

test_tty(_Config) ->
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
    Logger:set_level(?LEVEL_NOTSET),
    ?DEBUG("logging 2", []),
    ?INFO("logging 2", []),
    ?WARNING("logging 2", []),
    ?ERROR("logging 2", []),
    ?LOGGING(25, "logging 2", []),
    ?LOGGING(35, "logging 2", []),
    ?LOGGING(200, "logging 2", []),
    [begin
        ?LOGGING(188, "logging ~b", [N])
    end || N <- lists:seq(3, 100)],

    logging:stop(Logger:logger_name()),
    ok.

test_formatter(_Config) ->
    {ok, _} = logging:start("test_formatter"),
    Logger = logging:get_logger("test_formatter"),
    ok = Logger:set_level(?LEVEL_NOTSET),
    ok = Logger:set_formatter(""),
    ?DEBUG("logging 1", []),
    ok = Logger:set_formatter("(universal) xxx"),
    ?DEBUG("logging 2", []),
    ok = Logger:set_formatter("(universalxx)"),
    ?DEBUG("logging 3", []),
    ok = Logger:set_formatter("(localtime) (universal) (name) (levelno) (levelname) (module) (lineno) (pid) (ospid) (message)"),
    ?DEBUG("logging 4", []),
    ok = Logger:set_formatter("(universal) - (name) -(module):(lineno) - *(levelname)* - (message)"),
    ?DEBUG("logging 5", []),
    logging:stop(Logger:logger_name()),
    ok.

test_level(_Config) ->
    {ok, _} = logging:start("test_level"),
    Logger = logging:get_logger("test_level"),
    ok = Logger:set_formatter("(universal) - (name) - (levelno) (levelname) - (message)"),

    % about level
    ?LEVEL_WARNING = Logger:get_level(),
    ok = Logger:set_level(?LEVEL_NOTSET),
    ?LEVEL_NOTSET = Logger:get_level(),

    ?DEBUG("logging 1", []),
    ?INFO("logging 1", []),
    ?WARNING("logging 1", []),
    ?ERROR("logging 1", []),
    ?CRITICAL("logging 1", []),
    ?LOGGING(100, "logging 1", []),

    ok = Logger:add_level_name(?LEVEL_DEBUG, "*DEBUG*"),
    "*DEBUG*" = Logger:get_level_name(?LEVEL_DEBUG),
    ok = Logger:add_level_name(100, "GAME OVER"),
    "GAME OVER" = Logger:get_level_name(100),

    ?DEBUG("logging 2", []),
    ?INFO("logging 2", []),
    ?WARNING("logging 2", []),
    ?ERROR("logging 2", []),
    ?CRITICAL("logging 2", []),
    ?LOGGING(100, "logging 2", []),

    logging:stop(Logger:logger_name()),
    ok.

test_file(_Config) ->
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

-define(DO_LOGGING, 
        ?DEBUG("logging ~b", [N]),
        ?INFO("logging ~b", [N]),
        ?WARNING("logging ~b", [N]),
        ?ERROR("logging ~b", [N]),
        ?CRITICAL("logging ~b", [N]),
        ?LOGGING(100, "logging ~b", [N])
        ).

test_rotate_base(_Config) ->
    Formatter = "(name) - (levelno) (levelname) - (message)\n",
    Handler = {logging_handler_rotate, {"/tmp/test_rotate_1024_10", "my.log", 1024, 10}},
    {ok, _} = logging:start("test_rotate_base", ?LEVEL_NOTSET, Handler, Formatter),
    Logger = logging:get_logger("test_rotate_base"),

    [begin
        ?DO_LOGGING
    end || N <- lists:seq(1, 100)],
    logging:stop(Logger:logger_name()),
    ok.

test_rotate_more(_Config) ->
    Formatter = "(name) - (levelno) (levelname) - (message)\n",
    Size = 25452,
    Handler = {logging_handler_rotate, {"/tmp/test_rotate_50000_1", "my.log", 50000, 1}},
    {ok, _} = logging:start("test_rotate_more", ?LEVEL_NOTSET, Handler, Formatter),
    Logger = logging:get_logger("test_rotate_more"),

    [begin
        ?DO_LOGGING
    end || N <- lists:seq(1, 100)],
    
    Logger:set_handler(logging_handler_rotate, {"/tmp/test_rotate_3000_50", "my.log", 3000, 50}),
    [begin
        ?DO_LOGGING
    end || N <- lists:seq(1, 100)],
    logging:stop(Logger:logger_name()),

    Size = 
    lists:sum(
        [filelib:file_size(F) || F <- filelib:wildcard("/tmp/test_rotate_50000_1/*")]),
    Size = 
    lists:sum(
        [filelib:file_size(F) || F <- filelib:wildcard("/tmp/test_rotate_3000_50/*")]),
    ok.
