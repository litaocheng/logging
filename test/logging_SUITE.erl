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

suite() -> [].  

init_per_suite(Config) ->
    code:add_path("../ebin"),
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
        %test_file,
        %test_rotate,
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
    {ok, _} = logging:start("file"),
    Logger = logging:get_logger("file"),
    do_basic_logging(Logger),
    logging:stop(Logger:logger_name()),

    ok.

test_rotate(_Config) ->
    {ok, _} = logging:start("rotate"),
    Logger = logging:get_logger("rotate"),
    do_basic_logging(Logger),
    logging:stop(Logger:logger_name()),

    ok.

do_basic_logging(Logger) ->
    Logger:debug(?MODULE, ?LINE, "logging debug"),
    Logger:info(?MODULE, ?LINE, "logging info"),
    Logger:warning(?MODULE, ?LINE, "logging warning"),
    Logger:error(?MODULE, ?LINE, "logging error"),
    Logger:critical(?MODULE, ?LINE, "logging critical"),
    Logger:log(25, ?MODULE, ?LINE, "logging 25"),
    ok.

