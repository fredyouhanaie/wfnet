%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2023, Fred Youhanaie
%%% @doc
%%%
%%% common definitions
%%%
%%% @end
%%% Created :  7 Nov 2023 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------

-define(WFEMGR, wfnet_emgr).

%%--------------------------------------------------------------------

-define(Task_types, [wfenter, wftask, wfands, wfandj,
                     wfxors, wfxorj, wfexit]).

%%--------------------------------------------------------------------

-type task_id() :: integer().
%% task identifier

-type task_succ() :: task_id() | [task_id()] | [].
%% task successor

-type task_pred() :: task_id() | [task_id()] | [].
%% task predecessor

-type task_type() :: wfenter | wftask | wfands | wfandj |
                     wfxors | wfxorj | wfexit.
%% the basic task types

-type task_data() :: term().
%% task specific data

-type task() :: {task_type(), task_id(), task_succ(), task_data()}.
%% task specific tuples types

-type task_state() :: inactive | done | waiting | running.
%% task states

-type wf_state() :: no_wf | loaded | aborted | running | completed.
%% workflow states

%%--------------------------------------------------------------------

-record(task_rec,
        { id             :: task_id(),
          type           :: task_type(),
          state=inactive :: task_state(),
          pred           :: task_pred(),
          succ           :: task_succ(),
          data           :: term(),
          result={}      :: term()
        } ).
-type task_rec() :: #task_rec{}.
%% generic task record (for ETS)

%%--------------------------------------------------------------------
