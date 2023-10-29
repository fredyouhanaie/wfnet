%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang.anydata.co.uk>
%%% @copyright 2023, Fred Youhanaie
%%% @doc
%%%
%%% Run the function in a task.
%%%
%%% @end
%%% Created : 29 Oct 2023 by Fred Youhanaie <fyrlang.anydata.co.uk>
%%%-------------------------------------------------------------------
-module(wfnet_runner).

-export([run_task/2]).

-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------
%% @doc process the payload of a task.
%%
%% If the payload in `Data' is a function, it will be run in the
%% background, otherwise, a log message will be printed.
%%
%% @end
%%--------------------------------------------------------------------
-spec run_task(integer(), term()) -> ok.
run_task(Id, Data) ->
    erlang:spawn(fun () -> run_child(Id, Data) end),
    ok.

%%--------------------------------------------------------------------
%% @doc Spawn a child process given a `Module'/`Function'/`Args' triple.
%%
%% @end
%%--------------------------------------------------------------------
-spec run_child(integer(),
                {function(), list()} | {module(), function(), list()})
               -> term().
run_child(Id, {M, F, A}) ->
    Result = erlang:apply(M, F, A),
    wfnet:task_done(Id, Result);

run_child(Id, {Fun, Args}) when is_function(Fun) ->
    Result = erlang:apply(Fun, Args),
    wfnet:task_done(Id, Result);

run_child(Id, Data) ->
    ?LOG_NOTICE("non-function task (~p) completed (~p).", [Id, Data]),
    wfnet:task_done(Id, 0).

%%--------------------------------------------------------------------
