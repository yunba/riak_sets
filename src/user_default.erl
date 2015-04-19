-module(user_default).
-compile(export_all).

setup() ->
    sync:go(),
    code:add_pathsz(["../../deps/proper/ebin","../../.eunit", "../../deps/seqbind/ebin", "../../deps/uuid/ebin"]),
    lager:start(),
    lager:set_loglevel(lager_console_backend, warning),
    ok.

ping() ->
    riak_sets:ping().

ops() ->
        
    proper:module(set_ops_tests).

web() ->
   proper:module(web_tests). 
