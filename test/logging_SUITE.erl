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

end_per_suite(Config) ->
    ok.

init_per_testcase(Name, Config) ->
    Config.

end_per_testcase(Name, Config) ->
    ok.

all() -> 
    [
        test_logging,
        test_dummy
    ].

%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------
test_dummy(_Config) ->
    ok.

test_logging(Config) ->
    ok.
