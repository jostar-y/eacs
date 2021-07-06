-module(gen_acs).

-export([
   main/1
   , gen_tree/1
]).

-define(Spw, <<" ~〜,，:：.。;；-_=+*&^…%$#@!！|？?'‘’\"“”`·()[]{}（）【】「」/／\\、\n\t"/utf8>>).

main(Args) ->
   case Args of
      [SWFile, WriteDir] ->
         case file:open(SWFile, [read, raw, binary, {read_ahead, 65536}, {encoding, utf8}]) of
            {ok, IoDevice} ->
               {Goto, Output} = deal_every_sw(IoDevice, _Goto = #{0 => #{}}, _Output = #{}, _State = 0),
               file:close(IoDevice),
               Fail = gen_fail(Goto),
               gen_spw(WriteDir),
               gen_erl(WriteDir, Goto, Fail, Output);
            _Err ->
               io:format("gen_acs open the SWord file:~p error ~p~n", [SWFile, _Err])
         end;
      [Cmd, SWFile, FilterFile] when Cmd == "-F"; Cmd == "-f" ->
         load(acs_spw, [{get_spw, 1}], binary_to_list(spw_str())),
         case file:open(SWFile, [read, raw, binary, {read_ahead, 65536}, {encoding, utf8}]) of
            {ok, IoDevice} ->
               {Line, LineMap} = deal_every_fw(IoDevice, _UniqueMap = #{}, _LineMap = #{}, _Line = 1),
               file:close(IoDevice),
               file:delete(FilterFile),
               write_filter(1, Line, FilterFile, LineMap);
            _Err ->
               io:format("gen_acs open the Filter file:~p error ~p~n", [SWFile, _Err])
         end;
      _ ->
         io:format("Useage:\n\t1: to gen acs_tree.erl and acs_sqw.erl with  gen_acs  SWFile OuputDir\n\t2: to filter special word in SWFile and drop  repetitive words with gen_acs -f/F SWFile OuputDir\n"),
         ok
   end.

deal_every_fw(IoDevice, UniqueMap, LineMap, Line) ->
   case file:read_line(IoDevice) of
      {ok, DataStr} ->
         BinStr = binary:part(DataStr, 0, byte_size(DataStr) - 1),
         case BinStr =/= <<>> of
            true ->
               FilterBin = <<<<W/utf8>> || <<W/utf8>> <= BinStr, acs_spw:get_spw(W) /= true>>,
               case UniqueMap of
                  #{FilterBin := _} ->
                     deal_every_fw(IoDevice, UniqueMap, LineMap, Line);
                  _ ->
                     deal_every_fw(IoDevice, UniqueMap#{FilterBin => 1}, LineMap#{Line => FilterBin}, Line + 1)
               end;
            _ ->
               deal_every_fw(IoDevice, UniqueMap, LineMap, Line)
         end;
      eof ->
         {Line, LineMap};
      _Err ->
         io:format("gen_acs read the Filter file error ~p~n", [_Err])
   end.

write_filter(Line, Line, FilterFile, _LineMap) ->
   file:write_file(FilterFile, [], [append, sync]);
write_filter(CurLine, Line, FilterFile, LineMap) ->
   case LineMap of
      #{CurLine := BinStr} ->
         file:write_file(FilterFile, [BinStr, <<"\n">>], [append]),
         write_filter(CurLine + 1, Line, FilterFile, LineMap);
      _ ->
         write_filter(CurLine + 1, Line, FilterFile, LineMap)
   end.

deal_every_sw(IoDevice, Goto, Output, MaxState) ->
   case file:read_line(IoDevice) of
      {ok, DataStr} ->
         BinStr = binary:part(DataStr, 0, byte_size(DataStr) - 1),
         case BinStr =/= <<>> of
            true ->
               {NewGoto, NewState, NewMaxState} = add_goto(BinStr, Goto, 0, MaxState),
               NewOutput = Output#{NewState => e_acs:str_size(BinStr, 0)},
               deal_every_sw(IoDevice, NewGoto, NewOutput, NewMaxState);
            _ ->
               deal_every_sw(IoDevice, Goto, Output, MaxState)
         end;
      eof ->
         {Goto, Output};
      _Err ->
         io:format("gen_acs read the SWord file error ~p~n", [_Err])
   end.

%% 从字符串列表构建ac搜索树
gen_tree(BinStrList) ->
   %% 先构造 goto and output table
   {Goto, Output} = gen_goto_output(BinStrList, _Goto = #{0 => #{}}, _Output = #{}, _State = 0),
   %% 然后构造 fail table
   Fail = gen_fail(Goto),
   {Goto, Fail, Output}.

%% 构造 goto and output table
gen_goto_output([BinStr | Tail], Goto, Output, MaxState) ->
   case BinStr =/= <<>> of
      true ->
         {NewGoto, NewState, NewMaxState} = add_goto(BinStr, Goto, 0, MaxState),
         NewOutput = Output#{NewState => BinStr},
         gen_goto_output(Tail, NewGoto, NewOutput, NewMaxState);
      _ ->
         gen_goto_output(Tail, Goto, Output, MaxState)
   end;
gen_goto_output([], Goto, Output, _MaxState) ->
   {Goto, Output}.

%% 添加Goto 匹配状态转移项
add_goto(<<Word/utf8, Tail/binary>>, Goto, State, MaxState) ->
   #{State := Node} = Goto,
   case Node of
      #{Word := NextState} ->
         add_goto(Tail, Goto, NextState, MaxState);
      _ ->
         NewMaxState = MaxState + 1,
         NewNode = Node#{Word => NewMaxState},
         add_goto(Tail, Goto#{NewMaxState => #{}, State => NewNode}, NewMaxState, NewMaxState)
   end;
add_goto(<<>>, Goto, State, MaxState) ->
   {Goto, State, MaxState}.

%% 添加匹配Fail状态转移项
gen_fail(#{0 := Node} = Goto) ->
   gen_fail(maps:values(Node), Goto, _Fail = #{}).

%% 基于bfs搜索构造 Fail
gen_fail([State | Tail], Goto, Fail) ->
   #{State := Node} = Goto,

   %% 获取父节点的失败节点
   FailState = maps:get(State, Fail, 0),

   %% 子节点
   Kvs = maps:to_list(Node),

   %% 为子节点查找失败节点
   NewFail = add_fail(Kvs, FailState, Goto, Fail),

   %% 子节点入队列
   NewQueue = Tail ++ maps:values(Node),
   gen_fail(NewQueue, Goto, NewFail);
gen_fail([], _Goto, Fail) ->
   Fail.

%% 为节点构造失败指针
%% @param FailState 是当前节点的失败指针
add_fail([{Word, State} | Tail], FailState, Goto, Fail) ->
   NewFail = find_fail_node(Word, State, FailState, Goto, Fail),
   add_fail(Tail, FailState, Goto, NewFail);
add_fail([], _FailState, _Goto, Fail) ->
   Fail.

%% 为某个儿子节点构造失败指针
find_fail_node(Word, State, FailState, Goto, Fail) ->
   #{FailState := Node} = Goto,
   case Node of
      #{Word := TheFailState} ->
         %% 找到最近的失败节点的儿子节点拥有当前儿子节点的值，查找成功
         Fail#{State => TheFailState};
      _ ->
         case FailState =:= 0 of
            true ->
               %% 找不到，而且已经到了根节点，查找失败
               Fail;
            _ ->
               %% 找不到但是还没到根节点，继续往上找
               NewFailState = maps:get(FailState, Fail, 0),
               find_fail_node(Word, State, NewFailState, Goto, Fail)
         end
   end.

gen_head() ->
   <<"-module(acs_tree).\n\n-compile([deterministic, no_line_info]).\n\n-export([goto/1, fail_out/1]).\n\n">>.

gen_goto(Goto, StrAcc) ->
   Kvs = maps:to_list(Goto),
   SortKvs = lists:sort(Kvs),
   do_gen_goto(SortKvs, StrAcc).

do_gen_goto([], StrAcc) ->
   <<StrAcc/binary, "goto(_) -> undefined.\n\n">>;
do_gen_goto([{K, V}], StrAcc) ->
   case maps:size(V) of
      0 ->
         <<StrAcc/binary, "goto(_) -> undefined.\n\n">>;
      1 ->
         [TupleKV] = maps:to_list(V),
         <<StrAcc/binary, "goto(", (integer_to_binary(K))/binary, ") -> ", (iolist_to_binary(io_lib:format(<<"~w">>, [TupleKV])))/binary, ";\ngoto(_) -> undefined.\n\n">>;
      _ ->
         <<StrAcc/binary, "goto(", (integer_to_binary(K))/binary, ") -> ", (iolist_to_binary(io_lib:format(<<"~w">>, [V])))/binary, ";\ngoto(_) -> undefined.\n\n">>
   end;
do_gen_goto([{K, V} | SortKvs], StrAcc) ->
   case maps:size(V) of
      0 ->
         do_gen_goto(SortKvs, StrAcc);
      1 ->
         [TupleKV] = maps:to_list(V),
         NewStrAcc = <<StrAcc/binary, "goto(", (integer_to_binary(K))/binary, ") -> ", (iolist_to_binary(io_lib:format(<<"~w">>, [TupleKV])))/binary, ";\n">>,
         do_gen_goto(SortKvs, NewStrAcc);
      _ ->
         NewStrAcc = <<StrAcc/binary, "goto(", (integer_to_binary(K))/binary, ") -> ", (iolist_to_binary(io_lib:format(<<"~w">>, [V])))/binary, ";\n">>,
         do_gen_goto(SortKvs, NewStrAcc)
   end.

gen_fail_out([], _Fail, _Output, StrAcc) ->
   <<StrAcc/binary, "\nfail_out(_) -> {0, undefined}.">>;
gen_fail_out([State], Fail, Output, StrAcc) ->
   FailState = maps:get(State, Fail, 0),
   Pattern = maps:get(State, Output, undefined),
   case FailState /= 0 orelse Pattern /= undefined of
      true ->
         <<StrAcc/binary, "fail_out(", (integer_to_binary(State))/binary, ") -> ", (iolist_to_binary(io_lib:format(<<"~w">>, [{FailState, Pattern}])))/binary, ";\nfail_out(_) -> {0, undefined}.">>;
      _ ->
         <<StrAcc/binary, ";\nfail_out(_) -> {0, undefined}.">>
   end;
gen_fail_out([State | SortStates], Fail, Output, StrAcc) ->
   FailState = maps:get(State, Fail, 0),
   Pattern = maps:get(State, Output, undefined),
   case FailState /= 0 orelse Pattern /= undefined of
      true ->
         NewStrAcc = <<StrAcc/binary, "fail_out(", (integer_to_binary(State))/binary, ") -> ", (iolist_to_binary(io_lib:format(<<"~w">>, [{FailState, Pattern}])))/binary, ";\n">>,
         gen_fail_out(SortStates, Fail, Output, NewStrAcc);
      _ ->
         gen_fail_out(SortStates, Fail, Output, StrAcc)
   end.

-spec load(Module :: atom(), Export :: [{Fun :: atom(), Arity :: pos_integer()}], Str :: string()) -> {module, Module :: atom()} | {error, _}.
load(Module, Export, Str) ->
   {ok, Tokens, _EndLine} = erl_scan:string(Str),
   {ok, Forms} = erl_parse:parse_form(Tokens),
   NewForms = [{attribute, 1, module, Module}, {attribute, 2, export, Export}, Forms],
   {ok, _, Binary} = compile:forms(NewForms),
   code:load_binary(Module, "", Binary).

spw_head() ->
   <<"-module(acs_spw).\n\n-compile([deterministic, no_line_info]).\n\n-export([get_spw/1]).\n\n">>.

spw_str() ->
   GetSw = <<<<"get_spw(", (integer_to_binary(Spw))/binary, ") -> true;\n">> || <<Spw/utf8>> <= ?Spw>>,
   <<GetSw/binary, "get_spw(_) -> false.">>.

gen_spw(WriteDir) ->
   FileName = filename:join([WriteDir, "acs_spw.erl"]),
   file:write_file(FileName, <<(spw_head())/binary, (spw_str())/binary>>).

gen_erl(WriteDir, Goto, Fail, Output) ->
   HeadStr = gen_head(),
   GotoStr = gen_goto(Goto, HeadStr),
   FailStr = gen_fail_out(lists:sort(maps:keys(Goto)), Fail, Output, GotoStr),
   FileName = filename:join([WriteDir, "acs_tree.erl"]),
   file:write_file(FileName, FailStr).