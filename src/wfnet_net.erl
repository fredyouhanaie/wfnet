%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2023, Fred Youhanaie
%%% @doc
%%%
%%% A set of functions for processing Workflow Net definitions.
%%%
%%% @end
%%% Created : 28 Oct 2023 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(wfnet_net).

-export([read_file/1, load_ets/1, load_digraph/1, check_wf/1]).

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
%% @doc Check that a WF definition is valid.
%%
%% Check that the task tuples in a WF are valid.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_wf([task()]) -> ok | {error, term()}.
check_wf([]) ->
    ok;

check_wf([Task|WF]) ->
    try
        check_task(Task),
        check_wf(WF)
    catch throw:Error ->
            Error
    end;

check_wf(_WF) ->
    {error, bad_net}.

%%--------------------------------------------------------------------
%% run the test, raise exception if test fails, i.e. returns false.
%% We expect the variable `Task' to be defined when calling the macro.
%%
-define(Check_task(Test, Error),
        case Test of
            true ->  ok;
            false -> throw({error, {Error, Task}})
        end
       ).

%%--------------------------------------------------------------------
%% @doc Check a single task record.
%%
%% We check for:
%%
%% <ul>
%% <li>`wfenter' must have id 0 (for now)</li>
%% <li>`wfexit' must have identical `Id' and `Succ'</li>
%% <li>`wfands' must have a list as `Succ'</li>
%% <li>`wfxors' must have a list as `Succ'</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec check_task(task()) -> no_return().
check_task(Task={Type, Id, Succ, _Data}) ->
    ?Check_task(lists:member(Type, ?Task_types), bad_task_type),
    ?Check_task(is_integer(Id), bad_task_id),
    case Type of
        wfenter ->
            ?Check_task(Id==0, bad_task_wfenter);
        wfexit ->
            ?Check_task(Id==Succ, bad_task_wfexit);
        wfands ->
            ?Check_task(is_list(Succ), bad_task_succ);
        wfxors ->
            ?Check_task(is_list(Succ), bad_task_succ);
        _ ->
            ok
    end;

check_task(Task) ->
    throw({error, {bad_task, Task}}).

%%--------------------------------------------------------------------
%% @doc Load a workflow definition into an ETS table.
%%
%% The workflow definition, `WF', is converted to digraph, and the ETS
%% records are generated from the digraph vertices.
%%
%% Once the ETS table is populated, the digraph will be deleted.
%%
%% @end
%%--------------------------------------------------------------------
-spec load_ets([task()]) -> {ok, ets:table()} | {error, term()}.
load_ets(WF) ->
    case check_wf(WF) of
        ok ->
            case load_digraph(WF) of
                {ok, DG} ->
                    {ok, Tab} = digraph_to_ets(DG),
                    digraph:delete(DG),
                    {ok, Tab};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Create an ETS table from the WF digraph.
%%
%% @end
%%--------------------------------------------------------------------
-spec digraph_to_ets(digraph:graph()) -> {ok, ets:table()} | {error, term()}.
digraph_to_ets(G) ->
    Tab = ets:new(wfnet, [ordered_set]),
    Task_recs = [ vertex_to_task(G, V) || V <- digraph:vertices(G) ],
    ets:insert(Tab, Task_recs),
    {ok, Tab}.

%%--------------------------------------------------------------------
%% @doc convert a digraph vertex to a task record.
%%
%% @end
%%--------------------------------------------------------------------
-spec vertex_to_task(digraph:graph(), task_id()) -> task_rec().
vertex_to_task(G, Id) ->
    {Id, {Type, Data}} = digraph:vertex(G, Id),
    Pred = digraph:in_neighbours(G, Id),
    Succ = digraph:out_neighbours(G, Id),
    {Id, Type, Pred, Succ, Data, {}}.

%%--------------------------------------------------------------------
%% @doc load the workflow into a digraph.
%%
%% A new digraph containing the workflow is returned.
%%
%% It is the responsibility of the caller to delete the digraph when
%% no longer needed.
%%
%% @end
%%--------------------------------------------------------------------
-spec load_digraph(list()) -> {ok, digraph:graph()} | {error, term()}.
load_digraph(WF) ->
    case check_wf(WF) of
        ok ->
            G = digraph:new(),
            case add_tasks(WF, G) of
                ok ->
                    {ok, G};
                Err ->
                    digraph:delete(G),
                    Err
            end;
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
            add_vertex_succ(G, Id, Label, Succ);
        {Id, placeholder} -> %% task had been added as a placeholder
            add_vertex_succ(G, Id, Label, Succ);
        _ ->
            ?LOG_ERROR("duplicate task id (~p).", [Id]),
            {error, dup_task_id}
    end.

%%--------------------------------------------------------------------
%% @doc add a task vertex together with its successor edges
%%
%% We need to add a branching map for the `wfxors' tasks. This is to
%% decide which branch to take based on predecessor's result. The key
%% `default' can be used to catch any unmapped result values.
%%
%% The branching map will be stored in the `Data' item of the task
%% definition.
%%
%% If supplied `Data' is the empty tuple or map, i.e. `{}' or `#{}',
%% then a map will be generated where they map integers 0 to
%% `length(Succ)-1' to the list of `Id's in `Succ'.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_vertex_succ(digraph:graph(), task_id(), {task_type(), term()}, task_succ()) -> ok.
add_vertex_succ(G, Id, {wfxors, Data}, Succ) when Data=={} orelse Data==#{} ->
    %% set up the branching map, if not explicitly specified
    Data2 = maps:from_list(lists:enumerate(0, lists:flatten([Succ]))),
    add_vertex_succ(G, Id, {wfxors, Data2}, Succ);

add_vertex_succ(G, Id, Label, Succ) ->
    digraph:add_vertex(G, Id, Label),
    add_succ(G, Id, Succ).

%%--------------------------------------------------------------------
%% @doc Add one or more successor edges, and the vertex.
%%
%% The successor may be a single `task_id' or a list of `task_id's.
%%
%% We expect the vertex for `Id' to exist. If the vertex for the
%% successor id does not exist, we create a `placeholder' vertex for
%% it.
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
