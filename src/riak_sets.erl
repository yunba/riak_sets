-module(riak_sets).
-include("riak_sets.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         ping/0
        ]).


-ignore_xref([
              ping/0
             ]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx               = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList             = riak_core_apl:get_primary_apl(DocIdx, 1, riak_sets),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, riak_sets_vnode_master).



get_tree(Key) ->
    op({get_tree, Key}, {<<"set">>, Key}).
    
-compile(export_all).

%%%===================================================================
%%% API
%%%===================================================================
make_key(Set) ->
    {'set',term_to_binary(Set)}.

item_in_set(Set, Item) ->
    lager:debug("item_in_set(~p,~p)", [Set, Item]),
    case op({item_in_set, Set, Item}, make_key(Set)) of
        {ok, [Result, Result, Result]} ->
            Result;
        {ok, _Results} ->
            lager:info("Mismatched Data for {~p,~p} setting to present", [Set, Item]),
            add_to_set(Set, Item),
            true
    end.
                

add_to_set( Set, Item) ->
    lager:debug("add_to_set(~p,~p)", [Set, Item]),
    op({add_to_set, Set, Item}, make_key(Set)).



remove_from_set(Set,Item) ->
    lager:debug("remove_from_set(~p,~p)", [Set, Item]),
    op({remove_from_set, Set, Item}, make_key(Set)).
    

remove_from_set(Set) ->
    lager:debug("remove_from_set(~p)", [Set]),
    op({remove_from_set, Set}, make_key(Set)).

    
size(Set) ->
    lager:debug("size(~p)", [Set]),
    case op(3,3,{size, Set}, make_key(Set)) of
        {ok, L } when is_list(L) ->
            lists:max(L);
        X ->
            {error,X}
    end.


op(N, W, Op, Key) ->
    {ok, ReqId} = riak_sets_op_fsm:op(N,W,Op, Key),
    wait_for_reqid(ReqId).
    
op(Op, Key) ->
    N = 3,
    W = 3,
    op(N, W, Op, Key).
%% op(N,W,Op) ->
%%     riak_sets_op_fsm:op(N,W,Op).


    
wait_for_reqid(Id) ->
    wait_for_reqid(Id, 5000).

wait_for_reqid(Id, Timeout) ->
    receive {Id, Value} -> {ok, Value}
    after Timeout -> {error, timeout}
    end.
