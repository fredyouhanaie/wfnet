# Example workflows

There are a couple for, almost identical, workflow definitions.

The format is such that it can be easily read by the application, and
almost easy for a human to create it.

Of course, it will be easy to generate this from a more user-friendly
format, such as YAML or JSON, with a utility/compiler.

The format is a set of tuples as follows:

1. the first field is the type, one of the seven types wfenter,
   wfexit, wfands, wfandj, wfxors, wfxorj or wftask.
1. The second field is a unique integer id, `wfenter` should have
   id=0. A YAML compiler can generate these automatically.
1. The third field is the id of the successor(s), wfexit does not have
   successor, wfands and wfxors should have a list of IDs.
1. The fourth field is only present for wftask, it can be any term. If
   a triple `{Mod, Fun, Args}` or a pair `{Fun, Args}`, it will be run
   as a function. Otherwise, a log message will be generated.
