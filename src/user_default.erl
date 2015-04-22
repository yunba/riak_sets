-module(user_default).
-compile(export_all).



setup() ->
    code:add_pathsa(["../../ebin"]),
    code:add_pathsz(["../../deps/sync/ebin", "../../deps/proper/ebin", "../../deps/hackney/ebin", "../../.eunit", "../../deps/uuid/ebin", "../../deps/seqbind/ebin", "../../deps/recon/ebin"]),
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


con() ->

    clean(),
    proper:quickcheck(concurrency_eqc:prop_conc()),
    true.
    
con(N) ->
    
    clean(),
    proper:quickcheck(concurrency_eqc:prop_conc(), [N]),
    true.

merge(N) ->
    true = proper:quickcheck(riak_sets_tests:prop_merge_trees_empty_merge(),[N]),
    true = proper:quickcheck(riak_sets_tests:prop_merge_trees_self_merge(),[N]),
    true = proper:quickcheck(riak_sets_tests:prop_merge_trees_is_communicative(),[N]),
    ok.



    
