# `wfnet` - Embedded work flow engine for Erlang applications

[![Erlang CI](https://github.com/fredyouhanaie/wfnet/actions/workflows/erlang.yml/badge.svg)](https://github.com/fredyouhanaie/wfnet/actions/workflows/erlang.yml)

> **Note**: This project is the continuation of the entry for the
> Spawnfest 2023 competition:
> [wfnet1](https://github.com/spawnfest/wfnet1/).

`wfnet` provides a configuration based workflow enactment engine
within an Erlang application.

## About workflows

Here a workflow is an arrangement of tasks (activities) where each
task is only activated when, depending on the task type, one or all of
its predecessor(s) have terminated.

The concept, and implementation, of workflows here follows those
described on the [Workflow patterns](http://workflowpatterns.com/) web
site, more specifically the [basic control flow
patterns](http://workflowpatterns.com/patterns/control/).

Effectively, the workflow is a directed graph of tasks of one of the
following types:

`wftask`: Within `wfnet` tasks are Erlang functions, either an `{Mod,
Fun, Args}` triple, or a function expression.

* when enabled, the function is called, and the result obtained on
  completion.
* when terminated, its successor task is enabled.
* A task can only have one predecessor and one successor.

The special tasks are internally defined pseudo-tasks as follows:

* `wfenter`: this is the first task of the workflow, it has no
  predecessor. When enabled, it will intialise the workflow data and
  enable its successor.

* `wfexit`: this is the last task of the workflow, it has no
  successors. When enabled, it will clean up and return the data from
  the workflow

* `wfands`: this is an AND-split, aka parallel-split, it has a single
  predecessor, and one or more successors. When enabled, it will
  enable all its successors.

* `wfandj`: This is an AND-join, aka synchronization. It will only be
  enabled when ALL of its predecessors have terminated. When enabled,
  it will enable its successor.

* `wfxors`: This is an XOR-split, aka exclusive choice. When enabled,
  it will enable only one of its successors. The successor is chosen
  based on the termination result of its predecessor.

* `wfxorj`: This is an XOR-join, aka simple merge. It has one or more
  predecessors, and one successor. It is enabled as soon as one of its
  predecessors terminates.

## Build

    $ rebar3 compile
    $ rebar3 dialyzer

## Running with the sample workflows

    $ rebar3 shell
    > wfnet:start().
    > wfnet:load("Examples/sample1.dat").
    > wfnet:run_wf().
    > wfnet:stop().

The above can also be run with sample2.dat to see the function
execution. See the Examples directory for some explanation.

## The Command Line Interface

A CLI command (escript) is available. Use rebar3 to create the
command:

    $ rebar3 escriptize

It provides a number subcommands, with new ones being added:

    $ _build/default/bin/wfnet_cli -h

Use `info` to get summary of a workflow file:

    $ _build/default/bin/wfnet_cli info Examples/sample1.wf

Use `graph` to generate a `GraphViz dot` file, and view it:

    $ _build/default/bin/wfnet_cli graph Examples/sample1.wf >sample1-graph.dot
    $ xdot sample1-graph.dot

or

    $ _build/default/bin/wfnet_cli graph Examples/sample1.wf | xdot -

---
