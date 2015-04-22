%%%-------------------------------------------------------------------
%%% @author Zachary Kessin <>
%%% @copyright (C) 2015, Zachary Kessin
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2015 by Zachary Kessin <>
%%%-------------------------------------------------------------------


-module(set_ops_eqc).
-behaviour(proper_statem).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

value() -> weighted_union([
                           {50,quickcheck_util:uuid()},
                           {60,quickcheck_util:set_guid()},
                           {1, quickcheck_util:evil_real()}
                          ]).

partial_write(Set, Item) ->    
    DocIdx               = riak_core_util:chash_key({<<"set">>, term_to_binary(Set)}),
    PrefList             = riak_core_apl:get_primary_apl(DocIdx, 1, riak_sets),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, {add_to_set, Set,Item}, riak_sets_vnode_master).



command(_V) ->
    oneof([
	   %{call, ?MODULE,   partial_write,   [ quickcheck_util:set_guid(), quickcheck_util:set_guid()]},
           {call, riak_sets, add_to_set,      [ set_key(), set_value()]},
           {call, riak_sets, remove_from_set, [ set_key(), set_value()]},
           {call, riak_sets, item_in_set,     [ set_key(), set_value()]},
           {call, riak_sets, size,            [ set_key()]}
          ]).
    
set_value() -> value().
set_key()   -> value().


prop_run_commands() ->
    ?FORALL(Cmds,
            non_empty(commands(?MODULE)),
	    begin
                

		?WHENFAIL(
		   begin
		       quickcheck_util:print_cmds(Cmds,0),
		       true
		   end,

                   ?TIMEOUT(5000,            
                            begin
                                {_Start,End,Result} = run_commands(?MODULE,Cmds),
                                sets:fold(fun(_Item = {Key, Val},Acc) ->
                                                  riak_sets:remove_from_set(Key, Val),
                                                  Acc
                                          end, true, End),
                                Result == ok
                            end))
	    end).




initial_state() ->
    sets:new().


precondition(_,_) ->
    true.
next_state(S,_V, {call, _, add_to_set, [ Key, Value]}) ->
    sets:add_element({Key, Value}, S);
next_state(S,_V, {call, _, partial_write, [ Key, Value]}) ->
    sets:add_element({Key, Value}, S);
next_state(S,_V, _Cmd = {call, _, remove_from_set, [ Key, Value]}) ->
    sets:del_element({Key, Value}, S);
next_state(S,_V, _Cmd) ->
    S.

postcondition(_, Cmd, {error, timeout}) ->
    lager:error("TIMEOUT! ~p", [Cmd]),
    false;
postcondition(S,{call,_,item_in_set, [Key, Value]},Result) ->
    Result == sets:is_element({Key,Value},S);
postcondition(S, {call, _, size, [Key]}, Result) ->
    MCount = sets:fold(fun({K,_},Acc) when K == Key ->
                               Acc + 1;
                          (_, Acc) ->Acc
                       end, 0, S),
    ?assertEqual(MCount, Result),
    MCount == Result;
        
    
postcondition(_S,_Cmd,_Result) ->
    true.


clean() ->
    Sets = quickcheck_util:guids(),
    [riak_sets:remove_from_set(Key) || Key <- Sets],
    ok.


run_test_() ->
    application:ensure_all_started(lager),

    ?assertEqual( net_adm:ping('riak_sets4@127.0.0.1'),pong),

    {timeout, 3600,
     ?_assertEqual([],
                   begin

                       [RNode|_] = nodes(),
                       ?debugFmt("Starting proper tests on ~p", [RNode]),
                       rpc:call(RNode, proper, module, [?MODULE,[100,{to_file, user}]])
                   end
                  )
    }.
