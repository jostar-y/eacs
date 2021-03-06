-module(ac_tc).

-compile(inline).
-compile({inline_size, 128}).

-export([
   tc/1
   , tc/2
   , tc/3
   , ts/4
   , tm/5
   , cvr_time_unit/3
   , test/1
]).

%% Measure the execution time (in nanoseconds) for Fun().
-spec tc(Fun :: function()) -> {Time :: integer(), Value :: term()}.
tc(F) ->
   T1 = erlang:monotonic_time(),
   Val = F(),
   T2 = erlang:monotonic_time(),
   Time = cvr_time_unit(T2 - T1, native, nanosecond),
   {Time, Val}.

%% Measure the execution time (in nanoseconds) for Fun(Args).
-spec tc(Fun :: function(), Arguments :: [term()]) -> {Time :: integer(), Value :: term()}.
tc(F, A) ->
   T1 = erlang:monotonic_time(),
   Val = apply(F, A),
   T2 = erlang:monotonic_time(),
   Time = cvr_time_unit(T2 - T1, native, nanosecond),
   {Time, Val}.

%% Measure the execution time (in nanoseconds) for an MFA.
-spec tc(Module :: module(), Function :: atom(), Arguments :: [term()]) -> {Time :: integer(), Value :: term()}.
tc(M, F, A) ->
   T1 = erlang:monotonic_time(),
   Val = apply(M, F, A),
   T2 = erlang:monotonic_time(),
   Time = cvr_time_unit(T2 - T1, native, nanosecond),
   {Time, Val}.

-spec cvr_time_unit(Time :: integer(), FromUnit :: erlang:time_unit(), ToUnit :: erlang:time_unit()) -> ConvertedTime :: integer().
cvr_time_unit(Time, FromUnit, ToUnit) ->
   try
      FU =
         case FromUnit of
            native -> erts_internal:time_unit();
            perf_counter -> erts_internal:perf_counter_unit();
            nanosecond -> 1000 * 1000 * 1000;
            microsecond -> 1000 * 1000;
            millisecond -> 1000;
            second -> 1
         end,
      TU =
         case ToUnit of
            native -> erts_internal:time_unit();
            perf_counter -> erts_internal:perf_counter_unit();
            nanosecond -> 1000 * 1000 * 1000;
            microsecond -> 1000 * 1000;
            millisecond -> 1000;
            second -> 1
         end,
      case Time < 0 of
         true -> (TU * Time - (FU - 1)) div FU;
         _ -> TU * Time div FU
      end
   catch
      _ : _ ->
         erlang:error(badarg, [Time, FromUnit, ToUnit])
   end.

%% ????????????????????????LoopTimes???????????????
%% ut_tc:ts(LoopTimes, Module, Function, ArgsList).
%% ????????????????????????SpawnProcessesCount????????????????????? LoopTimes???????????????
%% ut_tc:tm(ProcessesCount, LoopTimes, Module, Function, ArgsList).

do_tc(M, F, A) ->
   T1 = erlang:monotonic_time(),
   apply(M, F, A),
   T2 = erlang:monotonic_time(),
   cvr_time_unit(T2 - T1, native, nanosecond).

distribution(List, Aver) ->
   distribution(List, Aver, 0, 0).
distribution([H | T], Aver, Greater, Less) ->
   case H > Aver of
      true ->
         distribution(T, Aver, Greater + 1, Less);
      false ->
         distribution(T, Aver, Greater, Less + 1)
   end;
distribution([], _Aver, Greater, Less) ->
   {Greater, Less}.

%% ===================================================================
%% test: one process test N times
%% ===================================================================
ts(LoopTime, M, F, A) ->
   {Max, Min, Sum, Aver, Greater, Less} = loop_ts(LoopTime, M, F, A, LoopTime, 0, 0, 0, []),
   io:format("=====================~n"),
   %io:format("execute Args:~p~n", [A]),
   io:format("execute Fun :~p~n", [F]),
   io:format("execute Mod :~p~n", [M]),
   io:format("execute LoopTime:~p~n", [LoopTime]),
   io:format("MaxTime: ~10s(ns) ~10s(s)~n", [integer_to_binary(Max), float_to_binary(Max / 1000000000, [{decimals, 6}, compact])]),
   io:format("MinTime: ~10s(ns) ~10s(s)~n", [integer_to_binary(Min), float_to_binary(Min / 1000000000, [{decimals, 6}, compact])]),
   io:format("SumTime: ~10s(ns) ~10s(s)~n", [integer_to_binary(Sum), float_to_binary(Sum / 1000000000, [{decimals, 6}, compact])]),
   io:format("AvgTime: ~10s(ns) ~10s(s)~n", [float_to_binary(Aver, [{decimals, 6}, compact]), float_to_binary(Aver / 1000000000, [{decimals, 6}, compact])]),
   io:format("Grar   : ~10s(cn) ~10s(~s)~n", [integer_to_binary(Greater), float_to_binary(Greater / LoopTime, [{decimals, 2}]), <<"%">>]),
   io:format("Less   : ~10s(cn) ~10s(~s)~n", [integer_to_binary(Less), float_to_binary(Less / LoopTime, [{decimals, 2}]), <<"%">>]),
   io:format("=====================~n").


loop_ts(0, _M, _F, _A, LoopTime, Max, Min, Sum, List) ->
   Aver = Sum / LoopTime,
   {Greater, Less} = distribution(List, Aver),
   {Max, Min, Sum, Aver, Greater, Less};
loop_ts(Index, M, F, A, LoopTime, Max, Min, Sum, List) ->
   Nanosecond = do_tc(M, F, A),
   NewSum = Sum + Nanosecond,
   if
      Max == 0 ->
         NewMax = NewMin = Nanosecond;
      Max < Nanosecond ->
         NewMax = Nanosecond,
         NewMin = Min;
      Min > Nanosecond ->
         NewMax = Max,
         NewMin = Nanosecond;
      true ->
         NewMax = Max,
         NewMin = Min
   end,
   loop_ts(Index - 1, M, F, A, LoopTime, NewMax, NewMin, NewSum, [Nanosecond | List]).


%% ===================================================================
%% Concurrency test: N processes each test one time
%% ===================================================================

tm(ProcCnt, LoopTime, M, F, A) ->
   loop_spawn(ProcCnt, M, F, A, self(), LoopTime),
   {Max, Min, Sum, Aver, Greater, Less} = collector(ProcCnt, 0, 0, 0, ProcCnt, []),
   io:format("=====================~n"),
   %io:format("execute Args:~p~n", [A]),
   io:format("execute Fun :~p~n", [F]),
   io:format("execute Mod :~p~n", [M]),
   io:format("execute LoopTime:~p~n", [LoopTime]),
   io:format("execute ProcCnts:~p~n", [ProcCnt]),
   io:format("MaxTime: ~10s(ns) ~10s(s)~n", [integer_to_binary(Max), float_to_binary(Max / 1000000000, [{decimals, 6}, compact])]),
   io:format("MinTime: ~10s(ns) ~10s(s)~n", [integer_to_binary(Min), float_to_binary(Min / 1000000000, [{decimals, 6}, compact])]),
   io:format("SumTime: ~10s(ns) ~10s(s)~n", [integer_to_binary(Sum), float_to_binary(Sum / 1000000000, [{decimals, 6}, compact])]),
   io:format("AvgTime: ~10s(ns) ~10s(s)~n", [float_to_binary(Aver, [{decimals, 6}, compact]), float_to_binary(Aver / 1000000000, [{decimals, 6}, compact])]),
   io:format("Grar   : ~10s(cn) ~10s(~s)~n", [integer_to_binary(Greater), float_to_binary(Greater / LoopTime, [{decimals, 2}]), <<"%">>]),
   io:format("Less   : ~10s(cn) ~10s(~s)~n", [integer_to_binary(Less), float_to_binary(Less / LoopTime, [{decimals, 2}]), <<"%">>]),
   io:format("=====================~n").


loop_spawn(0, _, _, _, _, _) ->
   ok;
loop_spawn(ProcCnt, M, F, A, CollectorPid, LoopTime) ->
   spawn_link(fun() -> worker(LoopTime, M, F, A, CollectorPid) end),
   loop_spawn(ProcCnt - 1, M, F, A, CollectorPid, LoopTime).

collector(0, Max, Min, Sum, ProcCnt, List) ->
   Aver = Sum / ProcCnt,
   {Greater, Less} = distribution(List, Aver),
   {Max, Min, Sum, Aver, Greater, Less};
collector(Index, Max, Min, Sum, ProcCnt, List) ->
   receive
      {result, Nanosecond} ->
         NewSum = Sum + Nanosecond,
         if
            Max == 0 ->
               NewMax = NewMin = Nanosecond;
            Max < Nanosecond ->
               NewMax = Nanosecond,
               NewMin = Min;
            Min > Nanosecond ->
               NewMax = Max,
               NewMin = Nanosecond;
            true ->
               NewMax = Max,
               NewMin = Min
         end,
         collector(Index - 1, NewMax, NewMin, NewSum, ProcCnt, [Nanosecond | List])
   after 1800000 ->
      io:format("execute time out~n"),
      ok
   end.

worker(LoopTime, M, F, A, CollectorPid) ->
   SumTime = loop_tm(LoopTime, M, F, A, 0),
   CollectorPid ! {result, SumTime}.

loop_tm(0, _, _, _, SumTime) ->
   SumTime;
loop_tm(LoopTime, M, F, A, SumTime) ->
   Microsecond = do_tc(M, F, A),
   loop_tm(LoopTime - 1, M, F, A, SumTime + Microsecond).

test(N) ->
   M1 = erlang:monotonic_time(),
   timer:sleep(N),
   M2 = erlang:monotonic_time(),
   Time = cvr_time_unit(M2 - M1, native, nanosecond),
   io:format("IMY******************111 ~p~n", [Time]),

   S1 = erlang:system_time(nanosecond),
   timer:sleep(N),
   S2 = erlang:system_time(nanosecond),
   io:format("IMY******************222 ~p~n", [S2 - S1]).



