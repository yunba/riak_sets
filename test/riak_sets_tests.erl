
%%%-------------------------------------------------------------------
%%% @author Zachary Kessin <>
%%% @copyright (C) 2015, Zachary Kessin
%%% @doc
%%%
%%% @end
%%% Created : 13 Apr 2015 by Zachary Kessin <>
%%%-------------------------------------------------------------------


-module(riak_sets_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


tvalue() ->
    weighted_union([{40,quickcheck_util:uuid()},
                    {60,vector(4,integer($a,$z))}]).

set_tree_item() ->
    ?LET({Key, Value}, 
         {tvalue(),non_empty(list(tvalue()))},
         begin
             {Key,gb_sets:from_list(Value)}
         end).

set_tree() ->
    ?LET(Items,
         [set_tree_item()],
         begin
             Sorted = lists:usort(Items),
             gb_trees:from_orddict(Sorted)
         end).
prop_merge_trees_is_communicative() ->
    ?FORALL({Tree1,Tree2},
            {set_tree(), set_tree()},
            ?WHENFAIL(
                   begin
                       ?debugFmt("Tree 1~n~p", [Tree1]),
                       ?debugFmt("Tree 2~n~p", [Tree2]),
                       false
                   end,
               begin
                   R1 = riak_sets_vnode:merge_trees(Tree2, Tree1),
                   R2 = riak_sets_vnode:merge_trees(Tree1, Tree2),
                   assertTreesEqual(R1,R2)
               end
              )).



prop_merge_trees_self_merge() ->
    ?FORALL(Tree,
            set_tree(),
            ?WHENFAIL(
               begin
                   ?debugFmt("Tree ~n~p~n",[Tree])
               end,
               begin
                   assertTreesEqual(Tree, riak_sets_vnode:merge_trees(Tree, Tree))
               end
           )).

prop_merge_trees_empty_merge() ->
    ?FORALL(Tree,
            set_tree(),
            ?WHENFAIL(
               ?debugFmt("Tree ~n~p~n",[Tree]),
               assertTreesEqual(Tree, riak_sets_vnode:merge_trees(Tree, gb_trees:empty()))
               
           )).

prop_set_size_timeout() ->
    ?FORALL(Tree,
            set_tree(),
            ?FORALL(Key,
                    oneof([tvalue(),
                           ?LET(Keys, gb_trees:keys(Tree),
                                oneof(Keys))]
                         ),
                    begin
                        {Time, {reply, Size, _}} = timer:tc(riak_sets_vnode, set_size, [Key, {}, Tree]),
            
                        ?assert(is_integer(Size)),
                        ?WHENFAIL(
                           begin
                               ?debugFmt("Size ~p",[Size])
                           end,
                           Time < 55)
                    end)).

prop_add_to_set_timeout() ->
    ?FORALL({Key, Value, State, Tree},
            {tvalue(), tvalue(), {state, {},{}}, set_tree()},
            begin
                {Time,_} = timer:tc(riak_sets_vnode, add_to_set_tree, [Key, Value, State, Tree]),
            
                Time < 30
            end).
   

prop_get_item_from_set_timeout() ->
    ?FORALL({Key, Value, State, Tree},
            {tvalue(), tvalue(), {state, {},{}}, set_tree()},
            begin
                {Time,_} = timer:tc(riak_sets_vnode, get_item_from_set, [Key, Value, State, Tree]),
                Time < 30
            end).
   


assertTreesEqual([_Tree1]) ->
    true;
assertTreesEqual([Tree1, Tree2|Rest]) ->
    assertTreesEqual(Tree1,Tree2) andalso
        assertTreesEqual([Tree2|Rest]).

assertTreesEqual(Tree1, Tree2) ->
    ?assertEqual(gb_trees:size(Tree1), gb_trees:size(Tree2)),
    Keys1 = gb_trees:keys(Tree1),
    Keys2 = gb_trees:keys(Tree2),
    ?assertEqual(Keys1,Keys2),
    lists:all(fun(Key) ->
                      S1 = gb_trees:get(Key,Tree1),
                      S2 = gb_trees:get(Key,Tree2),
                      gb_sets:is_empty(gb_sets:subtract(S1,S2))
              end, Keys1).




merge_test() ->
    S = {1,
         {"da39a3ee-5e6b-5b0d-b255-18901037926d",
          {2,
           {"da39a3ee-5e6b-5b0d-b255-18901037926d",
            {"aaaa",nil,nil},
            nil}},
          nil,nil}},
    assertTreesEqual(S,riak_sets_vnode:merge_trees(S, S)),
    true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run_test_() ->
    {timeout, 3600,
     ?_assertEqual([],proper:module(?MODULE,[100,{to_file, user}]))}.
