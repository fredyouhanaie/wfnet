%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2023, Fred Youhanaie
%%% @doc
%%%
%%% Command line utility for `wfnet'.
%%%
%%% @end
%%% Created : 20 Nov 2023 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(wfnet_cli).

%% API exports
-export([main/1]).

-include_lib("kernel/include/logger.hrl").

-include_lib("include/wfnet.hrl").

%%--------------------------------------------------------------------

-define(Log_level, notice).

-define(Version, "0.1.0").

-define(Opt_specs,
        [
         %%{Name,   ShortOpt,  LongOpt,    ArgSpec,        HelpMsg}
         {help,     $h,        "help",     undefined,      "Print help."},
         {version,  $v,        "version",  undefined,      "Print version."},
         {loglevel, $l,        "loglevel", {atom, notice}, "Set log level."}
        ]).

-define(Commands,
        [ {"graph", "Generate GraphViz DOT file"},
          {"info",  "Print information about workflow FILE"}
        ]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    logger:set_primary_config(level, ?Log_level),

    logger:set_handler_config(default, formatter, {logger_formatter, #{}}),

    ?LOG_DEBUG(#{func => ?FUNCTION_NAME, msg => "startup", args => Args} ),

    case getopt:parse(?Opt_specs, Args) of
        {error, {Reason, Data}} ->
            ?LOG_DEBUG(#{func => ?FUNCTION_NAME,
                         reason => Reason,
                         data => Data}),
            usage();

        {ok, {Parsed, Rest}} ->
            ?LOG_DEBUG(#{func => ?FUNCTION_NAME,
                         parsed => Parsed,
                         rest => Rest}),
            process_args(Parsed, Rest)
    end,

    timer:sleep(100),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

-define(Process_opt(Opt, Action),
        case (proplists:get_value(Opt, Opts)) of
            true -> Action, true;
            _    -> false
        end).

-spec process_args(proplists:proplist(), list()) -> ok|error.
process_args(Opts, Args) ->
    logger:set_primary_config(level, proplists:get_value(loglevel, Opts)),

    ?Process_opt(version, io:format("Version ~p.~n", [?Version]))
        orelse ?Process_opt(help, usage())
        orelse process_command(Args).

%%--------------------------------------------------------------------

usage() ->
    io:format("Version ~p.~n", [?Version]),
    getopt:usage(?Opt_specs, atom_to_list(?MODULE), "command ...",
                 [ {"command", "command to execute, e.g. graph, info ..."} ]),
    [ io:format("  ~10s  ~s~n", [Cmd, Desc])
      || {Cmd, Desc} <- ?Commands ],
    ok.

%%--------------------------------------------------------------------

-spec process_command(list()) -> ok|error.
process_command([]) ->
    ?LOG_ERROR("command expected.~n", []),
    usage(),
    error;

process_command([Cmd|Args]) ->
    do_command(list_to_atom(Cmd), Args).

%%--------------------------------------------------------------------

-spec do_command(atom(), list()) -> ok|error.
do_command(info, [File]) ->
    {ok, WF} = wfnet_file:read_file(File),
    G = wfnet_file:load_digraph(WF),
    N_tasks = length(digraph:vertices(G)),
    io:format("Workflow: ~p~n", [File]),
    io:format("   Tasks: ~p~n", [N_tasks]),
    ok;

do_command(graph, [File]) ->
    {ok, WF} = wfnet_file:read_file(File),
    G = wfnet_file:load_digraph(WF),
    dg_to_dot(G),
    ok;

do_command(Cmd, Args) ->
    io:format("cmd=~p, args=~p.~n", [Cmd, Args]),
    usage(),
    error.

%%--------------------------------------------------------------------

-spec dg_to_dot(digraph:graph()) -> ok.
dg_to_dot(G) ->
    Tasks = lists:sort(digraph:vertices(G)),
    io:format("digraph G {~n"),
    io:format("    graph [layout=dot rankdir=LR]~n"),
    io:format("~n"),
    print_tasks(G, Tasks),
    io:format("~n"),
    print_edges(G, lists:sort(digraph:edges(G))),
    io:format("}~n"),
    ok.

%%--------------------------------------------------------------------

-spec print_tasks(digraph:graph(), [task_id()]) -> ok.
print_tasks(_G, []) ->
    ok;

print_tasks(G, [Id|Rest]) ->
    print_node(digraph:vertex(G, Id)),
    print_tasks(G, Rest).

%%--------------------------------------------------------------------

-define(Node_fmt,
        #{wfenter => "    ~p [shape=ellipse]~n",
          wfexit  => "    ~p [shape=ellipse]~n",
          wfands  => "    ~p [shape=box]~n",
          wfandj  => "    ~p [shape=box]~n",
          wfxors  => "    ~p [shape=diamond]~n",
          wfxorj  => "    ~p [shape=diamond]~n",
          wftask  => "    ~p [shape=circle]~n"
         }).

-spec print_node({task_id(), term()}) -> ok.
print_node({Id, {wftask, _Data}}) ->
    print_node({Id, {wftask}});

print_node({Id, {Type}}) when is_map_key(Type, ?Node_fmt) ->
    io:format(map_get(Type, ?Node_fmt), [Id]),
    ok.

%%--------------------------------------------------------------------

-spec print_edges(digraph:graph(), [digraph:edge()]) -> ok.
print_edges(_G, []) ->
    ok;

print_edges(G, [E|Rest]) ->
    {E, Id1, Id2, _} = digraph:edge(G, E),
    io:format("    ~p -> ~p~n", [Id1, Id2]),
    print_edges(G, Rest).

%%--------------------------------------------------------------------
