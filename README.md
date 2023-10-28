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

---
