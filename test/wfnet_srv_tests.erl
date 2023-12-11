%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2023, Fred Youhanaie
%%% @doc
%%%
%%% Unit tests for the `wfnet_srv' gen_server module.
%%%
%%% @end
%%% Created : 11 Dec 2023 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(wfnet_srv_tests).

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

-define(Sample_3_file, "Examples/sample3.wf").
-define(Sample_4_file, "Examples/sample4.wf").

%%--------------------------------------------------------------------
%% typical state map as returned by `wfnet:info()'

%% should match the `-record(state, ...)' definition in `wfnet_srv'
%%
-define(SRV_state_0_init,
        #{ wf_state => no_wf,
           queue => [],
           tabid => undefined,
           task_state => #{},
           task_result => #{}
         }).

%% What we expect to see after loading any valid WF
%%
-define(SRV_state_1_loaded,
        #{ wf_state => loaded,
           queue => [],
           task_state => #{},
           task_result => #{}
         }).

%% sample3.wf
%%
-define(SRV_state_2_3_completed,
        #{ wf_state => completed,
           queue => [],
           task_state => #{0=>done, 1=>done, 2=>done},
           task_result => #{0=>0, 1=>0, 2=>0}
         }).

%% sample4.wf
%%
-define(SRV_state_2_4_completed,
        #{ wf_state => completed,
           queue => [],
           task_state => #{0=>done, 1=>done},
           task_result => #{0=>0, 1=>0}
         }).

%%--------------------------------------------------------------------
%% basic client tests
%%
client_test_() ->
    {"Start/Stop/Restart tests",
     [ {setup,
        fun() -> logger:set_primary_config(level, error) end, fun(_) -> ok end,
        [ {"start 1", ?_assertEqual(ok, wfnet:start()) },
          {"start 2", ?_assertEqual({error, {already_started, wfnet}}, wfnet:start()) },

          {"status/start", ?_assertEqual(?SRV_state_0_init, wfnet:info()) },

          {"stop 1", ?_assertEqual(ok, wfnet:stop()) },
          {"stop 2", ?_assertEqual({error, {not_started, wfnet}}, wfnet:stop()) },

          {"restart 1", ?_assertEqual(ok, wfnet:restart()) },
          {"status/restart", ?_assertEqual(?SRV_state_0_init, wfnet:info()) },
          {"restart 2", ?_assertEqual(ok, wfnet:restart()) }
        ] }
     ]}.

%%--------------------------------------------------------------------
%% server tests
%%
srv_setup() ->
    logger:set_primary_config(level, error),
    wfnet:start().

srv_cleanup(_) ->
    wfnet:stop().

srv_1_test_() ->
    {"Server tests",
     [ {"null workflow",
        {setup, fun srv_setup/0, fun srv_cleanup/1,
         [ { "initial state", ?_assertEqual(?SRV_state_0_init, wfnet:info()) },
           { "load WF", ?_assertEqual(ok, wfnet:load_wf([])) },
           { "run", ?_assertEqual({error, wfenter_not_found}, wfnet:run_wf()) }
         ] } },

       {"empty workflow (sample4)",
        {setup, fun srv_setup/0, fun srv_cleanup/1,
         [ { "initial state", ?_assertEqual(?SRV_state_0_init, wfnet:info()) },
           { "load WF", ?_assertEqual(ok, wfnet:load_file(?Sample_4_file)) },
           { "loaded state", ?_assertEqual(?SRV_state_1_loaded, wfnet_info()) },
           { "run WF", ?_assertEqual(ok, wfnet:run_wf()) },
           { "wait", ?_assertEqual(ok, timer:sleep(100)) },
           { "completed state", ?_assertEqual(?SRV_state_2_4_completed, wfnet_info()) }
         ] } },

       {"basic workflow (sample3)",
        {setup, fun srv_setup/0, fun srv_cleanup/1,
         [ { "initial state", ?_assertEqual(?SRV_state_0_init, wfnet:info()) },
           { "load WF", ?_assertEqual(ok, wfnet:load_file(?Sample_3_file)) },
           { "loaded state", ?_assertEqual(?SRV_state_1_loaded, wfnet_info()) },
           { "run WF", ?_assertEqual(ok, wfnet:run_wf()) },
           { "wait", ?_assertEqual(ok, timer:sleep(100)) },
           { "completed state", ?_assertEqual(?SRV_state_2_3_completed, wfnet_info()) }
         ] } }
     ] }.

%% return the server status without the ETS tabid
%%
wfnet_info() ->
    maps:without([tabid], wfnet:info()).

%%--------------------------------------------------------------------
