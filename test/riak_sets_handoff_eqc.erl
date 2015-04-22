%%%-------------------------------------------------------------------
%%% @author Zachary Kessin <>
%%% @copyright (C) 2015, Zachary Kessin
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2015 by Zachary Kessin <>
%%%-------------------------------------------------------------------


-module(riak_sets_handoff_eqc).
-behaviour(proper_statem).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

value() -> frequency([
		      {20,  quickcheck_util:set_guid()},
		      {80,  quickcheck_util:uuid()}

		     ]).


    
set_value() -> value().
set_key()   -> value().

prop_handoff() ->
        ?FORALL(Cmds,
            non_empty(commands(?MODULE)),
            ?WHENFAIL(
	                      begin
				  quickcheck_util:print_cmds(Cmds,0),
				  true
			      end,
               begin
		   {_Start,_End,_Result} = run_commands(?MODULE,Cmds),
		   true
	       end)).

cluster_node() ->
    oneof([node()|nodes()]).

handoff(Time ) ->
    io:format("Handoff~n"),
    riak_core_vnode_manager:force_handoff(),  
    timer:sleep(Time),
    true.


vnodes() ->
    DocIdx  = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 64, riak_sets),
    INodes = [INode|| {INode, _} <- PrefList],
    INodes.
    

command(_V) ->
    oneof([
	   {call, timer, sleep, [integer(0,10)]},
	   {call, ?MODULE, handoff, [integer(1,1000)]},
           {call, riak_sets, add_to_set,      [ set_key(), set_value()]}
	  ]).

    
initial_state() ->
    set_ops_eqc:clean(),
    sets:new().

next_state(S,_V, {call, _, add_to_set, [ Key, Value]}) ->
    sets:add_element({Key, Value}, S);
next_state(S,_V, _Cmd) ->
    S.

precondition(Set, _Cmd = {call, _, handoff, _}) ->
    
    all_present(Set);

precondition(_S,_Cmd) ->
    true.


postcondition(Set,{call, _, handoff, _},_Result) ->
    all_present(Set);
postcondition(_S,_Cmd,_Result) ->
    true.


all_present(Set) ->
    sets:fold(fun(_Item = {Key, Val},true) ->
		      riak_sets:item_in_set(Key, Val);
		 (_, false)  ->
		      false
	      end, true, Set).
   


run_test_() ->
    application:ensure_all_started(lager),

    ?assertEqual( net_adm:ping('riak_sets4@127.0.0.1'),pong),
    ?debugFmt("Nodes ~p", [nodes()]),
    {timeout, 3600,
     ?_assertEqual([],proper:module(?MODULE,[100,{to_file, user}]))}.
