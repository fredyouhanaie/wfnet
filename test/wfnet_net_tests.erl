%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2023, Fred Youhanaie
%%% @doc
%%%
%%% Unit tests for the file handling functions.
%%%
%%% @end
%%% Created : 12 Nov 2023 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(wfnet_net_tests).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------

%% this file should not be created!
-define(No_file, "Examples/no-file.wf").

%% should be an empty file (except for comments
-define(Sample_0_file, "Examples/sample0.wf").

%% the basic workflow sample
-define(Sample_1_file, "Examples/sample1.wf").
-define(Sample_1_ids, [0,1,2,3,4,5,6,7,8,9,10,11,12]).
-define(Sample_1_len, length(?Sample_1_ids)).

%%--------------------------------------------------------------------
%% basic file handling
%%
read_test_() ->
    {"File handling tests",
     [ {"missing file", ?_assertEqual({error, enoent}, wfnet_net:read_file(?No_file))},
       {"empty file",   ?_assertMatch({ok, []},        wfnet_net:read_file(?Sample_0_file))},
       {"sample1",      ?_assertMatch({ok, _},         wfnet_net:read_file(?Sample_1_file))}
     ] }.

%%--------------------------------------------------------------------
%% WF task validation tests
%%
check_wf_test_() ->
    {"Check WF tests",
     [ {"wf net good",  ?_assertEqual(ok, wfnet_net:check_wf([])) },
       {"wf net bad",   ?_assertEqual({error, bad_net}, wfnet_net:check_wf({})) },
       {"wfenter good", ?_assertEqual(ok, wfnet_net:check_wf([{wfenter, 0, 1, {}}]))},
       {"wfenter bad",  ?_assertMatch({error, {bad_task_wfenter, _}}, wfnet_net:check_wf([{wfenter, 1, 1, {}}]))},
       {"wfexit good",  ?_assertEqual(ok, wfnet_net:check_wf([{wfexit, 1, 1, {}}]))},
       {"wfexit bad",   ?_assertMatch({error, {bad_task_wfexit, _}}, wfnet_net:check_wf([{wfexit, 1, 2, {}}]))},
       {"wfands good",  ?_assertEqual(ok, wfnet_net:check_wf([{wfands, 1, [2,3], {}}]))},
       {"wfands bad",   ?_assertMatch({error, {bad_task_succ, _}}, wfnet_net:check_wf([{wfands, 1, 2, {}}]))},
       {"wfxors good",  ?_assertEqual(ok, wfnet_net:check_wf([{wfxors, 1, [2,3], {}}]))},
       {"wfxors bad",   ?_assertMatch({error, {bad_task_succ, _}}, wfnet_net:check_wf([{wfxors, 1, 2, {}}]))}
     ] }.

%%--------------------------------------------------------------------
%% digraph functions
%%
dg_setup() ->
    {ok, WF} = wfnet_net:read_file(?Sample_1_file),
    {ok, DG} = wfnet_net:load_digraph(WF),
    DG.

dg_cleanup(G) ->
    digraph:delete(G).
    
digraph_test_() ->
    {"Digraph tests",
     [ {setup,
        fun dg_setup/0,
        fun dg_cleanup/1,
        fun (G) ->
                [ {"digraph length", ?_assertEqual(?Sample_1_len, length(digraph:vertices(G)))},
                  {"digraph tasks",  ?_assertEqual(?Sample_1_ids, lists:sort(digraph:vertices(G)))}
                ]
        end },
       {"empty workflow",
        fun() ->
                {ok, DG} = wfnet_net:load_digraph([]),
                ?_assertEqual([], digraph:vertices(DG))
        end},
       {"bad workflow", ?_assertEqual({error, bad_net}, wfnet_net:load_digraph({})) },
       {"duplicate Id", ?_assertEqual({error, dup_task_id},
                                      wfnet_net:load_digraph([{wftask,1,2,aaa},{wftask,1,3,bbb}]))}
     ] }.

%%--------------------------------------------------------------------
%% ETS load functions
%%
ets_setup() ->
    {ok, WF} = wfnet_net:read_file(?Sample_1_file),
    {ok, Tab}  = wfnet_net:load_ets(WF),
    Tab.

ets_cleanup(Tab) ->
    ets:delete(Tab).

ets_test_() ->
    {"ETS table tests",
     [ {setup,
        fun ets_setup/0,
        fun ets_cleanup/1,
        fun (Tab) ->
                [ {"table length", ?_assertEqual(?Sample_1_len, length(ets:tab2list(Tab)))}
                ]
        end },
       {"empty workflow",
        fun() ->
                {ok, Tab} = wfnet_net:load_ets([]),
                ?_assertEqual([], ets:tab2list(Tab))
        end}
     ] }.

%%--------------------------------------------------------------------
