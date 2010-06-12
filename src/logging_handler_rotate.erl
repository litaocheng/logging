%%%----------------------------------------------------------------------
%%%
%%% @copyright litaocheng
%%%
%%% @author litaocheng <litaocheng@gmail.com>
%%% @doc a handler that writes all log to files in rotate style
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(logging_rotate_h).
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

init({Dir, File, MaxSize, MaxFile}) ->
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
    end.

log(_LogRecord, LogBin, State = #state{fd = Fd, fsize = FSize, 
            maxs = MaxSize, maxf = MaxFile, dir = Dir, file = FileName}) ->
    Reply = file:write(Fd, LogBin),

    FSizeNew = FSize + byte_size(LogBin),
    State2 =
    if FSizeNew >= MaxSize ->
            do_rotate(Dir, FileName, MaxFile),
            {ok, Fd2} = open_first_file(Dir, FileName),
            State#state{fd = Fd2, fsize = 0};
        true ->
            State#state{fsize = FSizeNew}
    end,
    {Reply, State2}.

terminate(_Reason, #state{fd = Fd}) ->
    file:sync(Fd),
    file:close(Fd).

%%
%% internal API
%%

do_rotate(Dir, FileName, MaxFile) ->
    LastFile = lists:concat([FileName, ".", MaxFile-1]),
    Files = [F || F <- filelib:wildcard(FileName++"*", Dir), is_valid(F, FileName, MaxFile)],

    Sorted = [CurLastFile | _] = lists:sort( fun(A, B) -> A > B end, Files),
    lists:foldl(
        fun(F, Prev) ->
                %% FIXME
                Old = filename:join([Dir, F]),
                New = filename:join([Dir, Prev]),
                file:rename(Old, New)
        end,
    CurLastFile, Sorted),
    ok.

%% filename, filename.1, filename.2, ..., filename.N-1
is_valid(FileName, FileName, _N) -> true;
is_valid(FileName ++ "." ++ Suffix, FileName, N) ->
    try list_to_integer(Suffix) < N
    catch _:_ ->
        false
    end;
is_valid(_, _FileName, _N) ->
    false.

open_first_file(Dir, File) ->
    FileFull = filename:join([Dir, File]),
    case file:open(FileFull, [raw, write, appen]) of
        {ok, Fd} ->
            {ok, Fd};
        Other ->
            throw(Other)
    end.
