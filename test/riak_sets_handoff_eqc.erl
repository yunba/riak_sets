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

command(_V) ->
    lager:notice("Comamnd Param ~p",[_V]),
    oneof([
	   {call, timer, sleep, [integer(0,10)]} 
	   
	  ]).
    

initial_state() ->
    {}.

precondition(_,_) ->
    true.

next_state(S,_V, _Cmd) ->
    S.

postcondition(_S,_Cmd,_Result) ->
    true.
