-module(ac_test).

-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

test1() ->
    ac_tc:ts(1000000, acs_tree, goto, [63870]).

test20() ->
    ac_tc:ts(1000000, e_acs, match_sw, [<<"fdsfads拉法叶舰fds淫秽ffdsfdsffdddd"/utf8>>]).

test21() ->
    ac_tc:ts(1000000, e_acs, replace_sw, [<<"fdsfads拉法叶舰fds淫秽ffdsfdsffdddd"/utf8>>]).

test22() ->
    ac_tc:ts(1000000, e_acs, is_has_sw, [<<"fdsfads拉法叶舰fds淫秽ffdsfdsffdddd"/utf8>>]).

test3(Cnt, BinStr) ->
    ac_tc:ts(Cnt, e_acs, match_sw, [BinStr]).

test31(Cnt, BinStr) ->
    ac_tc:ts(Cnt, e_acs, replace_sw, [BinStr]).

test4(Cnt, FileName) ->
    {ok, Data} = file:read_file(FileName),
    test3(Cnt, Data).

test41(Cnt, FileName) ->
    {ok, Data} = file:read_file(FileName),
    test31(Cnt, Data).