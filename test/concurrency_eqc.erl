%%%-------------------------------------------------------------------
%%% @author Zachary Kessin <>
%%% @copyright (C) 2015, Zachary Kessin
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2015 by Zachary Kessin <>
%%%-------------------------------------------------------------------


-module(concurrency_eqc).
-behaviour(proper_statem).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).




value() -> quickcheck_util:uuid().
    
set_value() -> value().
set_key()   -> value().

prop_conc() ->
    ?FORALL(Cmds,
            non_empty(commands(?MODULE)),
	    begin
		clean(),
		{_Start,{End,_}, Result} = run_commands(?MODULE,Cmds),
		
		?WHENFAIL(
		   begin
		       io:format("~n~nEnd Value ~p~n",[Result]),
		       quickcheck_util:print_cmds(Cmds,0),
		       
		       true
		   end,
		   begin

		       sets:fold(fun(_Item = {Key, Val},Acc) ->
					 riak_sets:remove_from_set(Key, Val),
					 Acc
				 end, true, End),
                       Result == ok
                   end)
	    end).


command(_V) ->

    oneof([
           {call, riak_sets, add_to_set,      [ set_key(), set_value()]},
           {call, riak_sets, remove_from_set, [ set_key(), set_value()]},
           {call, riak_sets, item_in_set,     [ set_key(), set_value()]},
	   {call, ?MODULE,   partial_write,   [ set_key(), set_value(), integer(1,2)]}
	  ]).
 

partial_write(Set, Item, W) ->    
    lager:debug("partial_write(~p,~p)", [Set, Item]),
    riak_sets:op(3, W, {add_to_set, Set, Item}, {<<"set">>,term_to_binary(Set)}).



initial_state() ->
    clean(),
    {sets:new(), sets:new()}.


precondition(_,_) ->
    true.


next_state({S1,S2}, _V, {call, _, add_to_set, [ Key, Value]}) ->
    {sets:add_element({Key, Value}, S1), S2};
next_state({S1,S2}, _V, {call, _, partial_write, [ Key, Value,_]}) ->
    {S1,sets:add_element({Key, Value}, S2)};
next_state({S1,S2}, _V, _Cmd = {call, _, remove_from_set, [ Key, Value]}) ->
    {sets:del_element({Key, Value}, S1),S2};
next_state(S,_V, _Cmd) ->
    S.


postcondition({S,_}, Cmd = {call,_,item_in_set, [Key, Value]}, _R = {ok,R = [Result,Result,Result]}) ->
    lager:warning("Result ~p -> ~p", [Cmd, R]),
    Result == sets:is_element({Key,Value},S);
postcondition(_S, Cmd = {call,_,item_in_set, [_Key, _Value]},  {ok, R = [_Result1,_Result2,_Result3]}) ->
    lager:warning("Result ~p -> ~p", [Cmd, R]),
    false;
postcondition({_S1,_S2}, _Cmd = {call,_,partial_write, [_Key, _Value,_]}, _R ) ->
    true;
postcondition(_S,_Cmd,_Result) ->
    true.



clean() ->
    Sets = quickcheck_util:guids(),
    [riak_sets:remove_from_set(Key) || Key <- Sets],
    ok.
