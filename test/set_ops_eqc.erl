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


command(_V) ->
    Backend = riak_sets,
    oneof([
	   {call, ?MODULE, partial_write,   [ quickcheck_util:set_guid(), quickcheck_util:set_guid(), 2]},
           {call, Backend, add_to_set,      [ set_key(), set_value()]},
           {call, Backend, remove_from_set, [ set_key(), set_value()]},
           {call, Backend, item_in_set,     [ set_key(), set_value()]},
           {call, Backend, size,            [ set_key()]}
          ]).
    
set_value() -> value().
set_key()   -> value().


prop_run_commands() ->
    ?FORALL(Cmds,
            non_empty(commands(?MODULE)),
	    begin
		{_Start,End,Result} = run_commands(?MODULE,Cmds),
		
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




initial_state() ->
    clean(),
    sets:new().


precondition(_,_) ->
    true.
next_state(S,_V, {call, _, add_to_set, [ Key, Value]}) ->
    sets:add_element({Key, Value}, S);
next_state(S,_V, {call, _, partial_write, [ Key, Value,_]}) ->
    sets:add_element({Key, Value}, S);
next_state(S,_V, _Cmd = {call, _, remove_from_set, [ Key, Value]}) ->
    sets:del_element({Key, Value}, S);
next_state(S,_V, _Cmd) ->
    S.

postcondition(S,{call,_,item_in_set, [Key, Value]},Result) ->
    Result == sets:is_element({Key,Value},S);
postcondition(S, {call, _, size, [Key]}, Result) ->
    MCount = sets:fold(fun({K,_},Acc) when K == Key ->
                               Acc + 1;
                          (_, Acc) ->Acc
                       end, 0, S),

    MCount == Result;
        
    
postcondition(_S,_Cmd,_Result) ->
    true.


partial_write(Set, Item, W) ->    
    lager:debug("partial_write(~p,~p)", [Set, Item]),
    riak_sets:op(2, W, {add_to_set, Set, Item}, {<<"set">>,term_to_binary(Set)}).


clean() ->
    Sets = quickcheck_util:guids(),
    [riak_sets:remove_from_set(Key) || Key <- Sets],
    ok.
