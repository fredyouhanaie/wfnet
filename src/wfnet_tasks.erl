%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2023, Fred Youhanaie
%%% @doc
%%%
%%% Functions for handling the workflow task states and results.
%%%
%%% All the data is kept in an ETS table, which is created with
%%% `wfnet_net:load_ets/1'.
%%%
%%% Each record in the table is of type `wfnet:task_rec()', see
%%% `include/wfnet.hrl' for details.
%%%
%%% `{ Id, Type, State, Pred, Succ, Data, Result }'
%%%
%%% @end
%%% Created : 14 Dec 2023 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(wfnet_tasks).

-export([get_state/2, put_state/3, all_states/1]).
-export([get_result/2, put_result/3, all_results/1]).


-include_lib("kernel/include/logger.hrl").

-include_lib("wfnet/include/wfnet.hrl").

%%--------------------------------------------------------------------
%% @doc Return the state field of a task
%%
%% In case of non-existent table or task record `error' will be
%% returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_state(ets:table(), task_id()) -> {ok, term()} | {error, term()}.
get_state(Tab_id, Id) ->
    get_task_elem(Tab_id, Id, #task_rec.state).

%%--------------------------------------------------------------------
%% @doc save a task's state
%%
%% @end
%%--------------------------------------------------------------------
-spec put_state(ets:table(), task_id(), term()) ->
          ok | {error, term()}.
put_state(Tab_id, Id, Task_state) ->
    put_task_elem(Tab_id, Id, {#task_rec.state, Task_state}).

%%--------------------------------------------------------------------
%% @doc Return the result field of a task
%%
%% In case of non-existent table or task record `error' will be
%% returned.
%%
%% The result field will be returned, whether the task has run or not.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_result(ets:table(), task_id()) -> {ok, term()} | {error, term()}.
get_result(Tab_id, Id) ->
    get_task_elem(Tab_id, Id, #task_rec.result).

%%--------------------------------------------------------------------
%% @doc save a task's result
%%
%% @end
%%--------------------------------------------------------------------
-spec put_result(ets:table(), task_id(), term()) ->
          ok | {error, term()}.
put_result(Tab_id, Id, Result) ->
    put_task_elem(Tab_id, Id, {#task_rec.result, Result}).

%%--------------------------------------------------------------------
%% @doc collect and return the states of all the tasks.
%%
%% @end
%%--------------------------------------------------------------------
-spec all_states(ets:table()) -> {ok, map()} | {error, term()}.
all_states(Tab_id) ->
    all_elements(Tab_id, #task_rec.state).

%%--------------------------------------------------------------------
%% @doc collect and return the results of all the tasks.
%%
%% @end
%%--------------------------------------------------------------------
-spec all_results(ets:table()) -> {ok, map()} | {error, term()}.
all_results(Tab_id) ->
    all_elements(Tab_id, #task_rec.result).

%%--------------------------------------------------------------------
%% internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc Get an element from a task record
%%
%% @end
%%--------------------------------------------------------------------
-spec get_task_elem(ets:table(), task_id(), integer()) ->
          {ok, term()} | {error, term()}.
get_task_elem(Tab_id, Id, Pos) ->
    case ets:info(Tab_id, name) of
        undefined ->
            {error, no_table};
        _ ->
            try
                {ok, ets:lookup_element(Tab_id, Id, Pos)}
            catch E ->
                {error, {bad_task_rec, Id}}
            end
    end.

%%--------------------------------------------------------------------
%% @doc update an element of a task record
%%
%% @end
%%--------------------------------------------------------------------
-spec put_task_elem(ets:table(), task_id(), {integer(), term()}) ->
          ok | {error, term()}.
put_task_elem(Tab_id, Id, {Pos, Val}) ->
    case ets:info(Tab_id, name) of
        undefined ->
            {error, no_table};
        _ ->
            U = ets:update_element(Tab_id, Id, {Pos, Val}),
            case U of
                true ->
                    ok;
                false ->
                    {error, bad_task_record}
            end
    end.

%%--------------------------------------------------------------------
%% @doc collect the requested elements of all the tasks.
%%
%% @end
%%--------------------------------------------------------------------
-spec all_elements(ets:table(), integer()) -> {ok, map()} | {error, term()}.
all_elements(Tab_id, Pos) ->
    case ets:info(Tab_id, name) of
        undefined ->
            {error, no_table};
        _ ->
            F = fun (Task_rec, Map) ->
                        Id = element(#task_rec.id, Task_rec),
                        El = element(Pos, Task_rec),
                        maps:put(Id, El, Map)
                end,
            {ok, ets:foldl(F, #{}, Tab_id)}
    end.

%%--------------------------------------------------------------------
