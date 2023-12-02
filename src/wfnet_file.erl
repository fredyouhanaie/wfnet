%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2023, Fred Youhanaie
%%% @doc
%%%
%%% A set of functions for processing workflow definitions.
%%%
%%% @end
%%% Created : 28 Oct 2023 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(wfnet_file).

-export([read_file/1, load_ets/1, load_digraph/1]).

%%--------------------------------------------------------------------

-include_lib("kernel/include/logger.hrl").

-include_lib("wfnet/include/wfnet.hrl").

%%--------------------------------------------------------------------
%% @doc read a workflow file and return it as a list of task tuples.
%%
%% @end
%%--------------------------------------------------------------------
-spec read_file(file:name_all()) -> {ok, [term()]} | {error, term()}.
read_file(File) ->
    case file:consult(File) of
        {ok, WF} ->
            {ok, WF};
        {error, Reason} ->
            ?LOG_ERROR("Could not read/parse file (~p) - ~p.", [File, Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Load a workflow definition into an ETS table.
%%
%% An ETS table is created and the task tuples are loaded into it.
%%
%% @end
%%--------------------------------------------------------------------
-spec load_ets([term()]) -> ets:table().
load_ets(WF) ->
    Tab = ets:new(wfnet, [named_table, {keypos, 2}, ordered_set]),
    insert_tasks(WF, Tab),
    Tab.

%%--------------------------------------------------------------------
%% @doc insert a list of tasks into the ETS table.
%%
%% @end
%%--------------------------------------------------------------------
-spec insert_tasks([term()], ets:table()) -> ets:table().
insert_tasks([], Tab) ->
    Tab;

insert_tasks([Task|WF], Tab) ->
    ets:insert(Tab, Task),
    insert_tasks(WF, Tab).

%%--------------------------------------------------------------------
%% @doc load the workflow into a digraph.
%%
%% A new digraph containing the workflow is returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec load_digraph(list()) -> {ok, digraph:graph()} | {error, term()}.
load_digraph(WF) ->
    G = digraph:new(),
    case add_tasks(WF, G) of
        ok ->
            {ok, G};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc add the tasks of a workflow to a digraph.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_tasks(list(), digraph:graph()) -> ok | {error, term()}.
add_tasks([], _G) ->
    ok;

add_tasks([{Type, Id, Succ, Data} | WF], G) ->
    case add_task(G, Id, {Type, Data}, Succ) of
        ok ->
            add_tasks(WF, G);
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc check and create a new task vertex.
%%
%% return error if a vertex with `Id' already exists, unless it is a
%% `placeholder'.
%%
%% A `placeholder' vertex for the `Id' may have already been created
%% when the predecessor was being created, in which case we just
%% overwrite the vetex label, `placeholder', with `Label'.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_task(digraph:graph(), task_id(), term(), task_succ()) ->
          ok | {error, dup_task_id}.
add_task(G, Id, Label, Succ) ->
    case digraph:vertex(G, Id) of
        false -> %% new task
            digraph:add_vertex(G, Id, Label),
            add_succ(G, Id, Succ),
            ok;
        {Id, placeholder} -> %% task had been added as a placeholder
            digraph:add_vertex(G, Id, Label),
            add_succ(G, Id, Succ),
            ok;
        _ ->
            ?LOG_ERROR("duplicate task id (~p).", [Id]),
            {error, dup_task_id}
    end.

%%--------------------------------------------------------------------
%% @doc Add one or more successor edges, and the vertex.
%%
%% The successor may be a single `task_id' or a list of `task_id's.
%%
%% We expect the vertex for `Id' to exist. If the vertex for the
%% successor id does not exist, we create a placeholder vertex for it.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_succ(digraph:graph(), task_id(), task_succ()) -> ok.
add_succ(_G, Id, Id) ->
    ok; %% no self-loop, unless it's `wfexit'

add_succ(G, Id, Succ) when is_list(Succ) ->
    [ add_succ(G, Id, S) || S <- Succ ],
    ok;

add_succ(G, Id, Succ) ->
    case digraph:vertex(G, Succ) of
        false ->
            digraph:add_vertex(G, Succ, placeholder);
        _ ->
            ok
    end,
    digraph:add_edge(G, Id, Succ),
    ok.

%%--------------------------------------------------------------------
