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
-module(wfnet_file_tests).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------

%% this file should not be created!
-define(No_file, "Examples/no-file.dat").

%% should be an empty file (except for comments
-define(Sample_0_file, "Examples/sample0.dat").

%% the basic workflow sample
-define(Sample_1_file, "Examples/sample1.dat").
-define(Sample_1_ids, [0,1,2,3,4,5,6,7,8,9,10,11,12]).
-define(Sample_1_len, length(?Sample_1_ids)).

%%--------------------------------------------------------------------
%% basic file handling
%%
read_test_() ->
    {"File handling tests",
     [ {"missing file", ?_assertEqual({error, enoent}, wfnet_file:read_file(?No_file))},
       {"empty file",   ?_assertMatch({ok, []},        wfnet_file:read_file(?Sample_0_file))},
       {"sample1",      ?_assertMatch({ok, _},         wfnet_file:read_file(?Sample_1_file))}
     ] }.

%%--------------------------------------------------------------------
%% digraph functions
%%
dg_setup() ->
    {ok, WF} = wfnet_file:read_file(?Sample_1_file),
    G = wfnet_file:load_digraph(WF),
    G.

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
       {"empty workflow", ?_assertEqual([], digraph:vertices(wfnet_file:load_digraph([])))},
       {"wfenter bad Id", ?_assertException(error, function_clause,
                                            digraph:vertices(wfnet_file:load_digraph([{wfenter,1,1}])))}
     ] }.

%%--------------------------------------------------------------------
