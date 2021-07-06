-module(e_acs).

-export([
   match_sw/1                 %% 返回匹配的敏感词列表
   , is_has_sw/1               %% 检查是否包含敏感词
   , replace_sw/1             %% 替换敏感词
   , is_has_rp_sw/1             %% 检测并替换敏感词
   , str_size/2               %% 获取utf8字符串的长度
]).

-define(RW, 42).              %% 替换字符的utf8code

%% *************************************** match_sw start ***************************************************************
-spec match_sw(BinStr :: binary()) -> [{StartIndex :: integer(), EndIndex :: integer(), Pattern :: binary()}].
match_sw(BinStr) ->
   do_match_ms(BinStr, 0, _Index = 1, _MatchList = []).

do_match_ms(<<>>, _, _Index, MatchList) ->
   MatchList;
do_match_ms(<<Word/utf8, Tail/binary>>, State, Index, MatchList) ->
   case acs_spw:get_spw(Word) of
      true ->
         do_match_ms(Tail, State, Index, MatchList);
      _ ->
         {NewState, NewMatchList} = match_word_ms(Word, State, Index, MatchList),
         do_match_ms(Tail, NewState, Index + 1, NewMatchList)
   end.

match_word_ms(Word, State, Index, MatchList) ->
   case acs_tree:goto(State) of
      undefined ->
         case State of
            0 ->
               {State, MatchList};
            _ ->
               {NextState, _} = acs_tree:fail_out(State),
               match_word_ms(Word, NextState, Index, MatchList)
         end;
      Node ->
         case Node of
            {Word, NextState} ->
               NewMatchList = get_output_ms(NextState, Index, MatchList),
               {NextState, NewMatchList};
            #{Word := NextState} ->
               NewMatchList = get_output_ms(NextState, Index, MatchList),
               {NextState, NewMatchList};
            _ ->
               case State of
                  0 ->
                     {State, MatchList};
                  _ ->
                     {NextState, _} = acs_tree:fail_out(State),
                     match_word_ms(Word, NextState, Index, MatchList)
               end
         end
   end.

get_output_ms(0, _Index, MatchList) ->
   MatchList;
get_output_ms(State, Index, MatchList) ->
   {FailState, Pattern} = acs_tree:fail_out(State),
   case Pattern of
      undefined ->
         get_output_ms(FailState, Index, MatchList);
      _ ->
         NewMatchList = [{Index - Pattern + 1, Pattern} | MatchList],
         get_output_ms(FailState, Index, NewMatchList)
   end.

%% *************************************** match_sw end   ***************************************************************
%% *************************************** is_has_sw start ***************************************************************
-spec is_has_sw(BinStr :: binary()) -> boolean().
is_has_sw(BinStr) ->
   do_match_is(BinStr, 0).

do_match_is(<<>>, _) ->
   false;
do_match_is(<<Word/utf8, Tail/binary>>, State) ->
   case acs_spw:get_spw(Word) of
      true ->
         do_match_is(Tail, State);
      _ ->
         case match_word_is(Word, State) of
            true ->
               true;
            NewState ->
               do_match_is(Tail, NewState)
         end
   end.

match_word_is(Word, State) ->
   case acs_tree:goto(State) of
      undefined ->
         case State of
            0 ->
               State;
            _ ->
               {NextState, _} = acs_tree:fail_out(State),
               match_word_is(Word, NextState)
         end;
      Node ->
         case Node of
            {Word, NextState} ->
               case get_output_is(NextState) of
                  false ->
                     NextState;
                  _ ->
                     true
               end;
            #{Word := NextState} ->
               case get_output_is(NextState) of
                  false ->
                     NextState;
                  _ ->
                     true
               end;
            _ ->
               case State of
                  0 ->
                     State;
                  _ ->
                     {NextState, _} = acs_tree:fail_out(State),
                     match_word_is(Word, NextState)
               end
         end
   end.

get_output_is(0) ->
   false;
get_output_is(State) ->
   {FailState, Pattern} = acs_tree:fail_out(State),
   case Pattern of
      undefined ->
         get_output_is(FailState);
      _ ->
         true
   end.
%% *************************************** match_sw end   ***************************************************************
%% *************************************** replace_sw start *************************************************************
-spec replace_sw(BinStr :: binary()) -> ReBinStr :: binary().
replace_sw(BinStr) ->
   TotalSize = byte_size(BinStr),
   case do_match_rs(BinStr, TotalSize - 1, _Index = 1, _State = 0, _MatchList = []) of
      [] ->
         BinStr;
      MatchBIMWs ->
         do_replace_sw(lists:reverse(MatchBIMWs), BinStr, TotalSize, _StartPos = 0, <<>>)
   end.

-spec is_has_rp_sw(BinStr :: binary()) -> {IsHasSw :: boolean(), ReBinStr :: binary()}.
is_has_rp_sw(BinStr) ->
   TotalSize = byte_size(BinStr),
   case do_match_rs(BinStr, TotalSize - 1, _Index = 1, _State = 0, _MatchList = []) of
      [] ->
         {false, BinStr};
      MatchBIMWs ->
         ReBinStr = do_replace_sw(lists:reverse(MatchBIMWs), BinStr, TotalSize, _StartPos = 0, <<>>),
         {true, ReBinStr}
   end.

do_replace_sw([], BinStr, TotalSize, StartPos, BinAcc) ->
   case TotalSize > StartPos of
      true ->
         <<BinAcc/binary, (binary:part(BinStr, StartPos, TotalSize - StartPos))/binary>>;
      _ ->
         BinAcc
   end;
do_replace_sw([{CurByteIndex, MatchWordCnt, _CurWordIndex} | MatchBIMWs], BinStr, TotalSize, StartPos, BinAcc) ->
   {EndByteIndex, FilterWs} = get_match_words(MatchWordCnt, BinStr, CurByteIndex, []),
   RPStr = unicode:characters_to_binary(FilterWs, utf8),
   case StartPos =< EndByteIndex of
      true ->
         NewBinAcc = <<BinAcc/binary, (binary:part(BinStr, StartPos, EndByteIndex - StartPos + 1))/binary, RPStr/binary>>;
      _ ->
         NewBinAcc = <<BinAcc/binary, RPStr/binary>>
   end,
   do_replace_sw(MatchBIMWs, BinStr, TotalSize, CurByteIndex + 1, NewBinAcc).

get_match_words(0, _BinStr, ByteIndex, FilterWs) ->
   {ByteIndex, FilterWs};
get_match_words(MatchWordCnt, BinStr, ByteIndex, FilterWs) ->
   Byte = binary:at(BinStr, ByteIndex),
   case Byte < 128 of
      true ->
         case acs_spw:get_spw(Byte) of
            true ->
               get_match_words(MatchWordCnt, BinStr, ByteIndex - 1, [Byte | FilterWs]);
            _ ->
               get_match_words(MatchWordCnt - 1, BinStr, ByteIndex - 1, [?RW | FilterWs])
         end;
      _ ->
         LUtf8Code = Byte band 63,
         LLByte = binary:at(BinStr, ByteIndex - 1),
         case LLByte bsr 6 == 2 of
            true ->
               LLUtf8Code = LLByte band 63,
               LLLByte = binary:at(BinStr, ByteIndex - 2),
               case LLLByte bsr 6 == 2 of
                  true ->
                     LLLUtf8Code = LLLByte band 63,
                     LLLLByte = binary:at(BinStr, ByteIndex - 3),
                     case LLLLByte bsr 6 == 2 of
                        true ->
                           LLLLUtf8Code = LLLLByte band 63,
                           LLLLLByte = binary:at(BinStr, ByteIndex - 4),
                           case LLLLLByte bsr 6 == 2 of
                              true ->
                                 LLLLLUtf8Code = LLLLLByte band 63,
                                 LLLLLLByte = binary:at(BinStr, ByteIndex - 5),
                                 LLLLLLUtf8Code = LLLLLLByte band 1,
                                 ReduceCnt = 6,
                                 FullWord = LLLLLLUtf8Code bsl 30 bor LLLLLUtf8Code bsl 24 bor LLLLUtf8Code bsl 18 bor LLLUtf8Code bsl 12 bor LLUtf8Code bsl 6 bor LUtf8Code;

                              _ ->
                                 LLLLLUtf8Code = LLLLLByte band 3,
                                 ReduceCnt = 5,
                                 FullWord = LLLLLUtf8Code bsl 24 bor LLLLUtf8Code bsl 18 bor LLLUtf8Code bsl 12 bor LLUtf8Code bsl 6 bor LUtf8Code
                           end;
                        _ ->
                           LLLLUtf8Code = LLLLByte band 7,
                           ReduceCnt = 4,
                           FullWord = LLLLUtf8Code bsl 18 bor LLLUtf8Code bsl 12 bor LLUtf8Code bsl 6 bor LUtf8Code
                     end;
                  _ ->
                     LLLUtf8Code = LLLByte band 15,
                     ReduceCnt = 3,
                     FullWord = LLLUtf8Code bsl 12 bor LLUtf8Code bsl 6 bor LUtf8Code
               end;
            _ ->
               LLUtf8Code = LLByte band 31,
               ReduceCnt = 2,
               FullWord = LLUtf8Code bsl 6 bor LUtf8Code
         end,
         case acs_spw:get_spw(FullWord) of
            true ->
               get_match_words(MatchWordCnt, BinStr, ByteIndex - ReduceCnt, [FullWord | FilterWs]);
            _ ->
               get_match_words(MatchWordCnt - 1, BinStr, ByteIndex - ReduceCnt, [?RW | FilterWs])
         end
   end.

deal_match_list([], CurByteIndex, MatchWordCnt, CurWordIndex) ->
   [{CurByteIndex, MatchWordCnt, CurWordIndex}];
deal_match_list([{_OldByteIndex, OldMatchWordCnt, OldWordIndex} | LeftMatchList] = OldMatchList, CurByteIndex, MatchWordCnt, CurWordIndex) ->
   CurStartIndex = CurWordIndex - MatchWordCnt,
   OldStartIndex = OldWordIndex - OldMatchWordCnt,
   if
      CurStartIndex >= OldWordIndex + 1 ->
         [{CurByteIndex, MatchWordCnt, CurWordIndex} | OldMatchList];
      CurStartIndex >= OldStartIndex ->
         [{CurByteIndex, CurWordIndex - OldStartIndex, CurWordIndex} | LeftMatchList];
      true ->
         deal_match_list(LeftMatchList, CurByteIndex, MatchWordCnt, CurWordIndex)
   end.

do_match_rs(<<>>, _TotalSize, _CurIndex, _State, MatchList) ->
   MatchList;
do_match_rs(<<Word/utf8, Tail/binary>>, TotalSize, CurIndex, State, MatchList) ->
   case acs_spw:get_spw(Word) of
      true ->
         do_match_rs(Tail, TotalSize, CurIndex, State, MatchList);
      _ ->
         {NewState, MatchCnt} = match_word_rs(Word, State, 0),
         case MatchCnt of
            0 ->
               do_match_rs(Tail, TotalSize, CurIndex + 1, NewState, MatchList);
            _ ->
               LeftSize = byte_size(Tail),
               NewMatchList = deal_match_list(MatchList, TotalSize - LeftSize, MatchCnt, CurIndex),
               do_match_rs(Tail, TotalSize, CurIndex + 1, NewState, NewMatchList)
         end
   end.

match_word_rs(Word, State, MatchCnt) ->
   case acs_tree:goto(State) of
      undefined ->
         case State of
            0 ->
               {State, MatchCnt};
            _ ->
               {NextState, _} = acs_tree:fail_out(State),
               match_word_rs(Word, NextState, MatchCnt)
         end;
      Node ->
         case Node of
            {Word, NextState} ->
               NewMatchCnt = get_output_rs(NextState, MatchCnt),
               {NextState, NewMatchCnt};
            #{Word := NextState} ->
               NewMatchCnt = get_output_rs(NextState, MatchCnt),
               {NextState, NewMatchCnt};
            _ ->
               case State of
                  0 ->
                     {State, MatchCnt};
                  _ ->
                     {NextState, _} = acs_tree:fail_out(State),
                     match_word_rs(Word, NextState, MatchCnt)
               end
         end
   end.

%% 获取当前字符最大匹配数
get_output_rs(0, MatchCnt) ->
   MatchCnt;
get_output_rs(State, MatchCnt) ->
   {FailState, Pattern} = acs_tree:fail_out(State),
   case Pattern of
      undefined ->
         get_output_rs(FailState, MatchCnt);
      _ ->
         case Pattern > MatchCnt of
            true ->
               get_output_rs(FailState, Pattern);
            _ ->
               get_output_rs(FailState, MatchCnt)
         end
   end.
% *************************************** replace_sw end   *************************************************************

str_size(<<>>, Cnt) ->
   Cnt;
str_size(<<_Word/utf8, Left/binary>>, Cnt) ->
   str_size(Left, Cnt + 1).