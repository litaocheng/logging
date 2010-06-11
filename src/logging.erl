%%%----------------------------------------------------------------------
%%%
%%% @copyright litaocheng
%%%
%%% @author litaocheng <litaocheng@gmail.com>
%%% @doc logging module
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(logging).
-author("litaocheng@gmail.com").
-vsn('0.1').
-behaviour(gen_server).
-include("logging.hrl").
-include("logging_internal.hrl").

-export([start/1, start_link/1, start_link/4]).
-export([get_logger/1]).

%% about formatter
-export([format_default_str/0, format_default_code/0, compile_format/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
        terminate/2, code_change/3]).

-export([valid_level/1, valid_module/1, valid_line/1, valid_message/1]).

-record(state, {
        name,
        level,
        hsl :: [handler()],     % handler state list
        format_str,
        format_code,
        level_map,
        ospid = os:getpid()
    }).

%% @doc start the logger with name
-spec start(log_name()) -> {'ok', pid()} | {'error', any()}.
start(Name) ->
    gen_server:start({local, logger_name(Name)}, Name, []).

%% @doc start the logger with name
-spec start_link(log_name()) -> {'ok', pid()} | {'error', any()}.
start_link(Name) ->
    gen_server:start_link({local, logger_name(Name)}, Name, []).

%% @doc start the logger with name and the other arguments
-spec start_link(log_name(), log_level(), log_handler(), log_formatter()) -> 
    {'ok', pid()} | {'error', any()}.
start_link(Name, Level, Handler, Formatter) ->
    gen_server:start_link({local, logger_name(Name)}, {Name, Level, Handler, Formatter}, []).

%% @doc return logger
get_logger(Name) ->
    logger:new(logger_name_existing(Name)).

%% @doc the format is (asctime) - (levelname) - (message)"
format_default_str() ->
    ?FORMATTER_DEFAULT.

format_default_code() ->
    compile_format(?FORMATTER_DEFAULT).  

%% compile the format string
%% (localtime)  - human-readable local time with form “2003-07-08 16:49:45,896” 
%% (universal)  - human-readable universal time with form “2003-07-08 16:49:45,896” 
%%                  (the numbers after the comma are millisecond portion of the time)
%% (name)       - name of logger
%% (levelno)    - numeric logging level for the message(10, 20, ...)
%% (levelname)  - text logging level name for the message("DEBUG", "INFO", ...)
%% (module)     - module name
%% (lineno)     - source line number where the logging call was issued
%% (pid)        - the erlang process identify which issued the log
%% (ospid)      - the os process indentify, (os:getpid())
%% (message)    - the logged message
%% 
%% for example:
%% "(localtime) - (name) - (message)"
compile_format(Fmt) ->
    L = re:split(Fmt, "[()]", [noteol, notempty, trim, notbol, {return, list}]),
    [compile_format_part(P) || P <- L].
    
%%------------------------------------------------------------------------------
%% gen_server callbacks
%%------------------------------------------------------------------------------
init({Name, Level, Handler = {Mod, Args}, Formatter}) ->
    valid_name(Name),
    valid_level(Level),
    valid_handler(Handler),
    valid_formatter(Formatter),
    {ok, #state{
        name = Name,
        level = Level,
        hsl = server_add_handler(Mod, Args, []),
        format_code = compile_format(Formatter),
        format_str = Formatter,
        level_map = default_level_map()
    }};
init(Name) ->    
    valid_name(Name),
    {Mod, Args} = default_handler(),
    {ok, #state{
        name = Name, 
        level = default_level(), 
        hsl = server_add_handler(Mod, Args, []),
        format_code = format_default_code(),
        format_str = format_default_str(),
        level_map = default_level_map()
    }}.

handle_call({add_level_name, Level, Name}, _From, State = #state{level_map = LevelMap}) ->
    LevelMap2 = do_update_level(Level, Name, LevelMap),
    {reply, ok, State#state{level_map = LevelMap2}};
handle_call({get_level_name, Level}, _From, State) ->
    LevelName = do_level_name(Level, State),
    {reply, LevelName, State};

handle_call({set_handler, Mod, Args}, _From, State = #state{hsl = HSL}) ->
    {Hib, Reply, HSL2} = server_set_handler(Mod, Args, HSL),
    handler_reply(Hib, Reply, HSL2, State);
handle_call({add_handler, Mod, Args}, _From, State = #state{hsl = HSL}) ->
    {Hib, Reply, HSL2} = server_add_handler(Mod, Args, HSL),
    handler_reply(Hib, Reply, HSL2, State);
handle_call({delete_handler, Mod, Args}, _From, State = #state{hsl = HSL}) ->
    {Reply, HSL2} = server_delete_handler(Mod, Args, HSL),
    {reply, Reply, State#state{hsl = HSL2}};
handle_call({all_handlers}, _From, State = #state{hsl = HSL}) ->
    Reply = server_all_handlers(HSL),
    {reply, Reply, State};

handle_call({set_formatter, FormStr, FormCode}, _From, State) ->
    {reply, ok, State#state{format_str = FormStr, format_code = FormCode}};
handle_call({get_formatter}, _From, State = #state{format_str = FormStr}) ->
    {reply, {ok, FormStr}, State};

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({log, Pid, Level, Mod, Line, Msg}, State) ->
    case is_enable(Level, State) of
        true ->
            do_logging(Level, Pid, Mod, Line, Msg, State);
        false ->
            ok
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.
    
%%------------------------------------------------------------------------------
%%
%% internal API
%%
%%------------------------------------------------------------------------------
%% return the logger name
logger_name(Name) when is_list(Name); is_atom(Name) ->
    list_to_atom(lists:concat([?PREFIX, Name])).

%% return the logger name, the atom must exist
logger_name_existing(Name) ->
    list_to_existing_atom(lists:concat([?PREFIX, Name])).

%%------------------------------------------------------------------------------
%% validate
%%------------------------------------------------------------------------------
valid_name(Name) when is_atom(Name); is_list(Name) -> true;
valid_name(_) -> throw({error, invalid_name}).

valid_level(Level) when is_integer(Level), Level >= 0 -> true;
valid_level(_) -> throw({error, invalid_level}). 

valid_module(Mod) when is_atom(Mod) -> true;
valid_module(Mod) when is_tuple(Mod), is_atom(element(1, Mod)) -> true;
valid_module(_) -> throw({error, invalid_module}).

valid_line(Line) when is_integer(Line), Line >= 0 -> true;
valid_line(_) -> throw({error, invalid_line}).

valid_message(Msg) when is_list(Msg); is_binary(Msg) -> true;
valid_message(_) -> throw({error, invalid_message}).

valid_handler({Mod, _Args}) when is_atom(Mod) -> true;
valid_handler(_) -> throw({error, invalid_handler}).

%% FIXME do more sanity check
valid_formatter(Formatter) when is_list(Formatter) -> true;
valid_formatter(_) -> throw({error, invalid_formatter}).

%%------------------------------------------------------------------------------
%% defaults 
%%------------------------------------------------------------------------------
default_level() -> ?WARNING.
default_handler() -> {logging_tty_h, []}.
default_level_map() -> 
    [{?NOTSET, "NOTSET"},
    {?DEBUG, "DEBUG"},
    {?WARNING, "WARNING"},
    {?ERROR, "ERROR"},
    {?CRITICAL, "CRITICAL"}].

%%------------------------------------------------------------------------------
%% levels
%%------------------------------------------------------------------------------
do_update_level(Level, Name, LevelMap) ->
    Fun =
    fun({L, N}) ->
            {L, N}
    end,
    FunNot =
    fun() ->
            [{Level, Name}]
    end,
    keyupdate(Level, 1, LevelMap, Fun, FunNot).

do_level_name(Level, #state{level_map = LevelMap}) ->
    case lists:keyfind(Level, 1, LevelMap) of
        false ->
            [?LOGLEVEL_STR, list_to_integer(Level)];
        {Level, LevelName} ->
            LevelName
    end.

%%------------------------------------------------------------------------------
%% handlers
%%------------------------------------------------------------------------------

server_set_handler(Mod, Args, HSL) ->
    % first remove all the other handlers
    [begin
        do_handler_terminate(M, remove_handler, S)
    end || #handler{module = M, state = S} <- HSL],
    
    % set up the new handler
    do_handler_init(Mod, Args, HSL).

server_add_handler(Mod, Args, HSL) ->
    % set up the new handler
    do_handler_init(Mod, Args, HSL).

server_delete_handler(Mod, Args, HSL) ->
    case split(Mod, HSL) of
        {Mod, Handler, HSL1} ->
            {do_handler_terminate(Mod, Args, Handler#handler.state), HSL1};
        error ->
            {{error, module_not_found}, HSL}
    end.

split(Mod, HSL) -> split(Mod, HSL, []).

split(Mod, [Ha|T], L) when Ha#handler.module =:= Mod ->
    {Mod, Ha, lists:reverse(L, T)};
split(Mod, [H|T], L) ->
    split(Mod, T, [H|L]);
split(_, [], _) ->
    error.

server_all_handlers(HSL) ->
    [Mod || #handler{module = Mod} <- HSL].

handler_reply(true, Reply, HSL, State) ->
    {reply, Reply, State#state{hsl = HSL}, hibernate};
handler_reply(false, Reply, HSL, State) ->
    {reply, Reply, State#state{hsl = HSL}}.

%% logging is enabled?
is_enable(Level, #state{level = Allowed}) when Level >= Allowed ->
    true;
is_enable(_, _) ->
    false.

%% do logging
do_logging(Level, Pid, Mod, Line, Msg, 
        State = #state{name = Name, ospid = OsPid, hsl = HSL, format_code = FormCode}) ->
    LogRecord = #log_record{
        time = now_sec(),
        name = Name,
        level = Level,
        module = Mod,
        lineno = Line, 
        pid = Pid,
        ospid = OsPid,
        message = Msg
    },
    LogBin = format_msg(FormCode, LogRecord, State),
    do_dispatch_handlers(LogRecord, LogBin, HSL).

%%  dispatch the log to handlers
do_dispatch_handlers(LogRecord, LogBin, [Handler | T]) ->
    case do_handler_log(LogRecord, LogBin, Handler) of
    {ok, Handler1} ->
        {Hib, NewHandlers} = do_dispatch_handlers(LogRecord, LogBin, T),
        {Hib, [Handler1|NewHandlers]};
    {ok, Handler1, hibernate} ->
        {_Hib, NewHandlers} = do_dispatch_handlers(LogRecord, LogBin, T),
        {true, [Handler1|NewHandlers]};
    no ->
        do_dispatch_handlers(LogRecord, LogBin, T)
    end;
do_dispatch_handlers(_LogRecord, _LogBin, []) ->
    {false, []}.

%% log handler handle the log
do_handler_log(LogRecord, LogBin, Handler = #handler{module = Mod, state = State}) ->
    case catch Mod:log(LogRecord, LogBin, State) of
        {ok, State1} ->
            {ok, Handler#handler{state = State1}};
        {ok, State1, hibernate} ->
            {ok, Handler#handler{state = State1}, hibernate};
        remove_handler ->
            do_handler_terminate(Mod, remove_handler, State),
            no;
        Other ->
            do_handler_terminate(Mod, {error, Other}, State),
            no
    end.
        
%% initialize the log handler
do_handler_init(Mod, Args, HSL) ->
    case catch Mod:init(Args) of
        {ok, State} ->
            {false, ok, [#handler{module = Mod, state = State} | HSL]};
        {ok, State, hibernate} ->
            {true, ok, [#handler{module = Mod, state = State} | HSL]};
        Other ->
            {false, Other, HSL}
    end.
    
%% terminate the log handler
do_handler_terminate(Mod, Args, State) ->
    catch Mod:terminate(Args, State).

%%------------------------------------------------------------------------------
%% formatter
%%------------------------------------------------------------------------------
%% compile the formatter
compile_format_part("localtime") ->
    {#log_record.time, fun do_human_local_time/2};
compile_format_part("universal") ->
    {#log_record.time, fun do_human_universal_time/2};
compile_format_part("name") ->
    #log_record.name;
compile_format_part("levelno") ->
    #log_record.level;
compile_format_part("levelname") ->
    {#log_record.level, fun do_level_name/2};
compile_format_part("module") ->
    #log_record.module;
compile_format_part("lineno") ->
    #log_record.lineno;
compile_format_part("pid") ->
    #log_record.pid;
compile_format_part("ospid") ->
    #log_record.ospid;
compile_format_part("message") ->
    #log_record.message;
compile_format_part(Other) when is_list(Other) ->
    {text, Other}.

format_msg(FormCode, LogRecord, State) ->
    [begin
        case Code of
            {text, Text} ->
                Text;
            {N, Fun} when is_function(Fun, 2) ->
                Fun(element(N, LogRecord), State);
            N when is_integer(N) ->
                element(N, LogRecord)
        end
    end || Code <- FormCode].

do_human_local_time({_MillS, _S, MicroS} = Now, _State) ->
    {{Y, Mon, Day}, {H, M, S}} = calendar:now_to_local_time(Now),
    lists:concat([Y, "-", two_chars(Mon), "-", two_chars(Day),
           $\s, two_chars(H), ":", two_chars(M), ":", two_chars(S),
           $\,, (MicroS div 10000000)]).

do_human_universal_time({_MillS, _S, MicroS} = Now, _State) ->
    {{Y, Mon, Day}, {H, M, S}} = calendar:now_to_universal_time(Now),
    lists:concat([Y, "-", two_chars(Mon), "-", two_chars(Day),
           $\s, two_chars(H), ":", two_chars(M), ":", two_chars(S),
           $\,, (MicroS div 10000000)]).

%%------------------------------------------------------------------------------
%% misc functions
%%------------------------------------------------------------------------------

%% key update
-spec keyupdate(any(), pos_integer(), list(), fun(), fun()) ->
    [_].
keyupdate(Key, N, List, Fun, FunNot) when is_integer(N), N > 0, 
        is_function(Fun, 1),
        is_function(FunNot, 0) ->
        keyupdate3(Key, N, List, Fun, FunNot).

keyupdate3(Key, Pos, [Tup|Tail], Fun, _FunNot) when element(Pos, Tup) =:= Key ->
    [Fun(Tup) | Tail];
keyupdate3(Key, Pos, [H|Tail], Fun, FunNot) ->
    [H | keyupdate3(Key, Pos, Tail, Fun, FunNot)];
keyupdate3(_Key, _Pos, [], _Fun, FunNot) -> % not found
    FunNot().

%% return the now in seconds
now_sec() ->
    {Mega, Sec, Micro} = erlang:now(),
    Mega * 1000000 + Sec + Micro div 1000000.

two_chars(N) when N =< 9 ->
        lists:concat([0, N]);
two_chars(N) when N =< 99 ->
        N.


%%------------------------------------------------------------------------------
%% EUNIT test
%%------------------------------------------------------------------------------

-ifdef(EUNIT).

compile_format_test() ->
    ?assertEqual([
            {text, []},
            {#log_record.time, fun do_human_local_time/2},
            {#log_record.time, fun do_human_universal_time/2},
            #log_record.name,
            #log_record.level,
            {#log_record.level, fun do_level_name/2},
            #log_record.module,
            #log_record.lineno,
            #log_record.pid,
            #log_record.ospid,
            #log_record.message],
            compile_format("(localtime)(universal)(name)(level)(levelname)"
            "(module)(lineno)(pid)(ospid)(message)")),
    ?assertEqual(["ok", #log_record.name, "yes"],
            compile_format("ok(name)yes")),

    ?assertEqual([
            {text, []},
            {#log_record.name, fun do_human_local_time/2},
            " - ",
            #log_record.name,
            " - ",
            #log_record.message],
            compile_format("(localtime) - (name) - (message)")),
    ok.

logger_name_test() ->
    ?assertEqual('logger_root', logger_name("root")),
    ?assertEqual('logger_root', logger_name_existing("root")),
    ?assertEqual('logger_root', logger_name(root)),
    ok.

valid_test() ->
    ?assert(valid_name(cheng)),
    %?assert(valid_name("cheng")),
    %?assertThrow({error, invalid_name}, valid_name(1234)),

    ?assert(valid_level(0)),
    ?assert(valid_level(1)),
    ?assertThrow({error, invalid_level}, valid_level(-1)),
    ?assertThrow({error, invalid_level}, valid_level(level)),

    ?assert(valid_module(test)),
    ?assert(valid_module('MODULE')),
    ?assert(valid_module({test, arg})),
    ?assertThrow({error, invalid_module}, valid_module(-1)),
    ?assertThrow({error, invalid_module}, valid_module("module")),

    ?assert(valid_line(0)),
    ?assert(valid_line(1)),
    ?assertThrow({error, invalid_line}, valid_line(-1)),
    ?assertThrow({error, invalid_line}, valid_line(line)),

    ?assert(valid_message(<<>>)),
    ?assert(valid_message(<<"message">>)),
    ?assert(valid_message("message")),
    ?assertThrow({error, invalid_message}, valid_message(-1)),
    ?assertThrow({error, invalid_message}, valid_message(message)),

    ?assert(valid_handler({logging_tty_h, []})),
    ?assert(valid_handler({logging_tty_h, 10})),
    ?assertThrow({error, invalid_handler}, valid_handler(module)),
    
    ?assert(valid_formatter("(localtime)")),
    ?assert(valid_formatter("some text (message)")),
    ?assertThrow({error, invalid_formatter}, valid_formatter(formatter)),

    ok.

human_time_test() ->
    ?assert(is_list(do_human_local_time(now(), #state{}))),
    ?assert(is_list(do_human_universal_time(now(), #state{}))),
    ok.


-endif.
