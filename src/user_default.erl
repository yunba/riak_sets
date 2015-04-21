-module(user_default).
-compile(export_all).

setup() ->
    code:add_pathsa(["../../ebin"]),
    code:add_pathsz(["../../deps/sync/ebin", "../../deps/proper/ebin", "../../deps/hackney/ebin", "../../.eunit", "../../deps/uuid/ebin", "../../deps/seqbind/ebin"]),
    sync:go(),
    lager:set_loglevel(lager_console_backend, notice),
    ok.


ops(N) ->
    proper:quickcheck(set_ops_eqc:prop_run_commands(), [N]),
    true.

ops() ->
    clean(),
    proper:quickcheck(set_ops_eqc:prop_run_commands()),
    true.
    

handoff() ->
    clean(),
    proper:quickcheck(riak_sets_handoff_eqc:prop_handoff()),
    true.

clean() ->
    set_ops_eqc:clean().

