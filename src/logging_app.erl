%%%----------------------------------------------------------------------
%%%
%%% @copyright litaocheng
%%%
%%% @author litaocheng <litaocheng@gmail.com>
%%% @doc logging app and supervisor callback
%%% @end
%%%
%%%----------------------------------------------------------------------
-module(logging_app).
-author("litaocheng@gmail.com").
-vsn('0.1').
-include("logging.hrl").

-behaviour(application).
-behaviour(supervisor).

-export([start/0]).
-export([start/2, stop/1]).
-export([init/1]).


%% @doc start the application from the erl shell
-spec start() -> 'ok' | {'error', any()}.
start() ->
    ensure_apps(),
    ?DEBUG2("start the ~p application~n", [?MODULE]),
    logging_ctl:init(),
    application:start(logging).

%% @doc the application start callback
-spec start(Type :: any(), Args :: any()) -> any().
start(_Type, _Args) ->
    ?DEBUG2("start the supervisor sup ~n", []),
    supervisor:start_link({local, logging_sup}, ?MODULE, []).

%% @doc the application  stop callback
stop(_State) ->
    ok.

%% @doc supervisor callback
init(_Args) -> 
    ?DEBUG2("init supervisor~n", []),
    
    Stragegy = {one_for_one, 10, 10},

    ModServer = {logging_server, {logging_server, start_link, []},
                permanent, 2000, worker, [logging_server]},

    {ok, {Stragegy, [
                    ModServer 
                    ]}
    }.

%%
%% internal API
%%

%% first ensure some apps must start
ensure_apps() ->
    application:start(sasl),
    ok.
