
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

value() -> quickcheck_util:set_guid().

    %% frequency([{100, oneof([int(),binary(), char(), uuid(), vector(10, char())])},
    %%            {10, evil_real()},
    %%            {50, set_guid()}]).



    
set_value() -> value().
set_key()   -> value().
prop_save_and_exists() ->
    ?FORALL({Key, Value},
            {set_key(), set_value()},
            begin
                Backend = riak_sets,
                Backend:start_link(),
               
                gen_server:cast(riak_sets,'reset'),
                false =  Backend:item_in_set( Key, Value),
                Backend:add_to_set( Key, Value),
                true  =  Backend:item_in_set( Key, Value),
                true

            end).

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
    ?FORALL({TREE1,TREE2},
            {set_tree(), set_tree()},
            ?WHENFAIL(
                   begin
                       ?debugFmt("Tree 1~n~p", [TREE1]),
                       ?debugFmt("Tree 2~n~p", [TREE2]),
                       false
                   end,
               begin
                   R1 = riak_sets_vnode:merge_trees(TREE2, TREE1),
                   R2 = riak_sets_vnode:merge_trees(TREE1, TREE2),
                   assertTreesEqual(R1,R2)
               end
              )).


prop_merge_trees_self_merge() ->
    ?FORALL(TREE,
            set_tree(),
            ?WHENFAIL(
               begin
                   ?debugFmt("Tree ~n~p~n",[TREE])
               end,
               begin
                   assertTreesEqual(TREE, riak_sets_vnode:merge_trees(TREE, TREE))
               end
           )).

prop_merge_trees_empty_merge() ->
    ?FORALL(TREE,
            set_tree(),
            ?WHENFAIL(
               ?debugFmt("Tree ~n~p~n",[TREE]),
               assertTreesEqual(TREE, riak_sets_vnode:merge_trees(TREE, gb_trees:empty()))
               
           )).

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

run_te3st_() ->
    application:ensure_all_started(lager),
    code:add_pathz("../apps/setref/ebin"),
    {timeout, 3600,
     ?_assertEqual([],proper:module(?MODULE,[100,{to_file, user}]))}.
