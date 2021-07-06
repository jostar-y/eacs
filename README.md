e_acs
=====

    基于ac算法实现的快速高效的敏感词匹配,检查,过滤功能, 另外特殊字符不参与敏感词匹配,检查和替换, 替换是会按照原位置保留

Build
-----

    $ rebar3 escriptize   ->   gen_acs
    $ rebar3 compile

Uses
-----

    敏感词预处理 去除特殊字符和去掉重复的敏感词 （SWordFile 和 OutputDirFile) 可以同名
         脚本生成：./gen_acs -f/-F SWordFile OutputDirFile
        函数调用： gen_acs:main(["-f"/"-F", SWordFile, OutputDirFile])
    创建 acs_tree.erl
        脚本生成：./gen_acs SWordFile OutputDir
        函数调用： gen_acs:main([SWordFile, OutputDir])
    匹配 检查 过滤 敏感词
        e_acs:match_sw/1               %% 返回匹配的敏感词列表
        e_acs:is_has_sw/1               %% 检查是否包含敏感词
        e_acs:replace_sw/1             %% 替换敏感词
        e_acs:is_has_rp_sw/1             %% 检测并替换敏感词

性能
----- 

    实际测试中
        基于在一个2万敏感词构造的ac状态树中测试 匹配耗时为 50-100ns 一个字 算下来1秒可以匹配上千万的文本
    测试示例(测试前先注释掉测试代码打印的参数和e_acs match_sw匹配输出的列表构造): 
        下载了一个比较火的动漫小说 吞噬星空.txt  
        查看该小说有多少字
        {ok, DataStr} = file:read_file("吞噬星空.txt"),
        e_acs:str_size(DataStr, 0).           -> 5729268
        测试匹配
        ac_test:test4(100, "./src/test/吞噬星空.txt").
        =====================
        execute Fun :match_sw
        execute Mod :e_acs
        execute LoopTime:100
        MaxTime:  450278766(ns)   0.450279(s)
        MinTime:  428782619(ns)   0.428783(s)
        SumTime: 4345761036(ns)   43.45761(s)
        AvgTime: 434576103.(ns)   0.434576(s)
        Grar   :         43(cn)       0.43(%)
        Less   :         57(cn)       0.57(%)
        =====================
        ok
        
        It's really fast!!!

算法说明
-----

[算法说明](https://www.cnblogs.com/cmmdc/articles/7337611.html)

参考
-----

[aho-corasick](https://github.com/wudeng/aho-corasick)

## Attention

本项目是对 [Sis](https://github.com/SisMaker) 的 [eAcs](http://47.108.26.175:53000/SisMaker/eAcs) 项目进行了代码风格调整，使之更贴近社区规范
