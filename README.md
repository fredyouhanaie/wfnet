# `wfnet` - Embedded work flow engine for Erlang applications

`wfnet` provides a configuration based workflow enactment engine
within an Erlang application.

## Historical notes

* This is the second attempt at the same problem.
* The previous attempt was during Spawnfest 2018, however, that was
  aborted due to other issues not related to the competition.
* No code was committed during the 2018 competion.
* The current effort is a completely new rethink and no code from the
  2018 attempt will be used here.

## General description

The application will be built in stages, starting with a bare engine,
followed by proper OTP based supervised servers, and extended as time
permits.

## Build

    $ rebar3 compile
    $ rebar3 dialyzer

## Running with the sample workflows

    $ rebar3 shell
    > wfnet:start().
    > wfnet:load("Examples/sample1.dat").
    > wfnet:run_wf().
    > wfnet:stop().

## Project status

The development was carried out on a Linux system (archlinux) with Erlang/OTP 26.1.1.

There is much to be done yet:

* Unit testsv :-()
* the `and-join` and `xor-split` are just dummy place holders.
* We need more realistic examples.

---
