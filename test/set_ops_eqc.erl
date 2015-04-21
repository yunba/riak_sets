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



value() -> quickcheck_util:uuid().


    
set_value() -> value().
set_key()   -> value().


prop_run_commands() ->
    ?FORALL(Cmds,
            non_empty(commands(?MODULE)),
            ?WHENFAIL(
               begin
                   quickcheck_util:print_cmds(Cmds,0),
                   true
               end,
               begin
                   {_Start,End,Result} = run_commands(?MODULE,Cmds),
                   lager:info("Set Contents ~p", [sets:to_list(End)]),

                   sets:fold(fun(_Item = {Key, Val},Acc) ->
                                     riak_sets:remove_from_set(Key, Val),
                                     Acc
                              end, true, End),
                                              

                       Result == ok
                   end)).


command(_) ->
    Backend = riak_sets,
    oneof([

           {call, Backend, add_to_set,      [ set_key(), set_value()]},
           {call, Backend, remove_from_set, [ set_key(), set_value()]},
           {call, Backend, item_in_set,     [ set_key(), set_value()]}
          ]).
        


initial_state() ->
    clean(),
    sets:new().


precondition(_,_) ->
    true.
next_state(S,_V, {call, _, add_to_set, [ Key, Value]}) ->
    sets:add_element({Key, Value}, S);
next_state(S,_V, _Cmd = {call, _, remove_from_set, [ Key, Value]}) ->
    sets:del_element({Key, Value}, S);
next_state(S,_V, _Cmd) ->
    S.

postcondition(S,{call,_,item_in_set, [Key, Value]},Result) ->
    Result == sets:is_element({Key,Value},S);
postcondition(_S,_Cmd,_Result) ->
    true.



clean() ->
    Sets = quickcheck_util:guids(),
    [riak_sets:remove_from_set(Key) || Key <- Sets],
    ok.
