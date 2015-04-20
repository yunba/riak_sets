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


-compile(export_all).

%%%===================================================================
%%% API
%%%===================================================================
item_in_set(Set, Item) ->
    lager:debug("item_in_set(~p,~p)", [Set, Item]),
    op({item_in_set, Set, Item}, {<<"set">>,term_to_binary(Set)}).
    

add_to_set( Set, Item) ->
    lager:debug("add_to_set(~p,~p)", [Set, Item]),
    op({add_to_set, Set, Item}, {<<"set">>,term_to_binary(Set)}).



remove_from_set(Set,Item) ->
    lager:debug("remove_from_set(~p,~p)", [Set, Item]),
    op({remove_from_set, Set, Item}, {<<"set">>,term_to_binary(Set)}).

remove_from_set(Set) ->
    lager:debug("remove_from_set(~p)", [Set]),
    op({remove_from_set, Set}, {<<"set">>,term_to_binary(Set)}).

    
size(Set) ->
    lager:debug("remove_from_set(~p)", [Set]),
    op({size, Set}, {<<"set">>,term_to_binary(Set)}).



op(Op, Key) ->
    N = 3,
    W = 3,
    {ok, ReqId} = riak_sets_op_fsm:op(N,W,Op, Key),
    wait_for_reqid(ReqId).

%% op(N,W,Op) ->
%%     riak_sets_op_fsm:op(N,W,Op).


    
wait_for_reqid(Id) ->
    wait_for_reqid(Id, 5000).

wait_for_reqid(Id, Timeout) ->
    receive {Id, Value} -> {ok, Value}
    after Timeout -> {error, timeout}
    end.
