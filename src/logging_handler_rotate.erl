%%%----------------------------------------------------------------------
%%%
%%% @copyright litaocheng
%%%
%%% @author litaocheng <litaocheng@gmail.com>
%%% @doc a handler that writes all log to files in rotate style
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(logging_handler_rotate).
-author("litaocheng@gmail.com").
-vsn('0.1').
-behaviour(logging_handler).

-export([init/1, log/3, terminate/2]).

-record(state, {
        dir,
        file,
        maxs,
        maxf,
        fd,
        fsize = 0
    }).

-define(TRACE(F, D), io:format(F, D)).

init({Dir, File, MaxSize, MaxFile}) when is_integer(MaxSize), MaxSize >= 1024, 
                            is_integer(MaxFile), MaxFile > 0 ->
    ?TRACE("init dir ~p", [Dir]),
    case filelib:ensure_dir(filename:join([Dir, "*"])) of
        ok ->
            do_rotate(Dir, File, MaxFile),
            {ok, Fd} = open_first_file(Dir, File),
            {ok, #state{dir = Dir,
                    file = File,
                    maxs = MaxSize,
                    maxf = MaxFile,
                    fd = Fd
                }};
        Other ->
            Other
    end;
init(_) ->
    {error, invalid_arg_for_rotate}.

log(_LogRecord, LogBin, State = #state{fd = Fd, fsize = FSize, 
            maxs = MaxSize, maxf = MaxFile, dir = Dir, file = FileName}) ->
    Reply = file:write(Fd, LogBin),
    FSizeNew = FSize + iolist_size(LogBin),
    State2 =
    if FSizeNew >= MaxSize ->
            close_file(Fd),
            do_rotate(Dir, FileName, MaxFile),
            {ok, Fd2} = open_first_file(Dir, FileName),
            State#state{fd = Fd2, fsize = 0};
        true ->
            State#state{fsize = FSizeNew}
    end,
    %?TRACE("state after ~p~n", [State2]),
    {Reply, State2}.

terminate(_Reason, #state{fd = Fd}) ->
    ?TRACE("rotate handler terminate:~p ~n", [_Reason]),
    close_file(Fd).

%%
%% internal API
%%

do_rotate(Dir, FName, MaxFile) ->
    FNameLen = length(FName),
    ValidFiles = [F || F <- filelib:wildcard(FName++"*", Dir), 
        is_valid(F, FName, FNameLen, MaxFile)],
    case lists:sort( fun(A, B) -> A > B end, ValidFiles) of
        [] ->
            ok;
        [CurLastFile | Others] = Sorted ->
            ?TRACE("sorted is ~p~n", [Sorted]),
            case is_last_filename(CurLastFile, FName, MaxFile) of
                true ->
                    % delete the lastest file
                    file:delete(filename:join([Dir, CurLastFile])),
                    % move other files
                    move_files(Dir, Others, CurLastFile);
                false -> 
                    % get the next filename
                    Next = next_filename(CurLastFile, FName, MaxFile),
                    % move other files
                    move_files(Dir, Sorted, Next)
            end
    end.
                    
move_files(Dir, List, Init) ->
    lists:foldl(
        fun(F, Prev) ->
                %% FIXME
                Old = filename:join([Dir, F]),
                New = filename:join([Dir, Prev]),
                ?TRACE("~s => ~s~n", [Old, New]),
                ok = file:rename(Old, New),
                F
        end,
    Init, List).

%% if the filename is the last valid filename
is_last_filename(FName, FName, 1) ->
    true;
is_last_filename(F, FName, Max) ->
    F =:= lists:concat([FName, ".", Max-1]).

%% get the next valid filename 
next_filename(F, FName, Max) ->
    N = filename_index(F, FName, length(FName)),
    case filename_index(F, FName, length(FName)) of
        N when N >= 0, N < Max-1 -> % mustbe
            lists:concat([FName, ".", N+1])
    end.

%% filename, filename.1, filename.2, ..., filename.N-1
is_valid(F, FName, FNameLen, Max) ->
    case filename_index(F, FName, FNameLen) of
        N when N < Max, N >= 0 ->
            true;
        _ ->
            false
    end.

%% return the filename index 
%% 0 for first file, -1 standfor invalid index
filename_index(FName, FName, _FNameLen) ->
    0;
filename_index(F, _FName, FNameLen) ->
    try lists:nthtail(FNameLen+1, F) of
        Suffix ->
            try_to_integer(Suffix)
    catch 
        _:_ ->
            -1
    end.

try_to_integer(Str) ->
    try list_to_integer(Str)
    catch _:_ ->
        -1
    end.

close_file(Fd) ->
    file:sync(Fd),
    file:close(Fd).

open_first_file(Dir, File) ->
    FileFull = filename:join([Dir, File]),
    ?TRACE("open file ~p~n", [FileFull]),
    case file:open(FileFull, [raw, write, append]) of
        {ok, Fd} ->
            {ok, Fd};
        Other ->
            throw(Other)
    end.
