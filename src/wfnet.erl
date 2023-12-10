%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2023, Fred Youhanaie
%%% @doc
%%%
%%% Client functions for the workflow server.
%%%
%%% @end
%%% Created : 28 Oct 2023 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(wfnet).

-export([start/0, stop/0, restart/0]).
-export([load_file/1, load_wf/1, run_wf/0, task_done/2]).
-export([info/0]).

-include_lib("wfnet/include/wfnet.hrl").

%%--------------------------------------------------------------------
%% @doc start the application.
%%
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok | {error, term()}.
start() ->
    application:start(wfnet).

%%--------------------------------------------------------------------
%% @doc stop the application.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(wfnet).

%%--------------------------------------------------------------------
%% @doc restart the application
%%
%% @end
%%--------------------------------------------------------------------
-spec restart() -> ok | {error, term()}.
restart() ->
    case stop() of
        ok ->
            start();
        {error, {not_started, wfnet}} ->
            start();
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc load a workflow from file.
%%
%% @end
%%--------------------------------------------------------------------
-spec load_file(file:name_all()) -> ok | {error, term()}.
load_file(Filename) ->
    wfnet_srv:load_file(Filename).

%%--------------------------------------------------------------------
%% @doc load a workflow from a list of tasks.
%%
%% @end
%%--------------------------------------------------------------------
-spec load_wf([task()]) -> ok | {error, term()}.
load_wf(WF) ->
    wfnet_srv:load_wf(WF).

%%--------------------------------------------------------------------
%% @doc run the currently loaded workflow.
%%
%% @end
%%--------------------------------------------------------------------
-spec run_wf() -> ok | {error, term()}.
run_wf() ->
    wfnet_srv:run_wf().

%%--------------------------------------------------------------------
%% @doc Tell the server that a task has completed.
%%
%% @end
%%--------------------------------------------------------------------
-spec task_done(integer(), term()) -> ok | {error, term()}.
task_done(Id, Result) ->
    wfnet_srv:task_done(Id, Result).

%%--------------------------------------------------------------------
%% @doc get status info from the server
%%
%% @end
%%--------------------------------------------------------------------
-spec info() -> term().
info() ->
    wfnet_srv:wf_info().

%%--------------------------------------------------------------------
