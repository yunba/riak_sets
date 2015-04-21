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

    DocIdx               = riak_core_util:chash_key({<<"set">>,term_to_binary(Set)}),
    PrefList             = riak_core_apl:get_primary_apl(DocIdx, 1, riak_sets),
    [{IndexNode, _Type}] = PrefList,
    
    riak_core_vnode_master:sync_spawn_command(IndexNode, {item_in_set, Set, Item} , riak_sets_vnode_master).

    

add_to_set( Set, Item) ->
%    lager:info("add_to_set(~p,~p)", [Set, Item]),
    DocIdx               = riak_core_util:chash_key({<<"set">>,term_to_binary(Set)}),
    PrefList             = riak_core_apl:get_primary_apl(DocIdx, 1, riak_sets),
    [{IndexNode, _Type}] = PrefList,

    riak_core_vnode_master:sync_spawn_command(IndexNode, {add_to_set, Set, Item} , riak_sets_vnode_master).


remove_from_set(Set,Item) ->
    DocIdx               = riak_core_util:chash_key({<<"set">>,term_to_binary(Set)}),
    PrefList             = riak_core_apl:get_primary_apl(DocIdx, 1, riak_sets),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, {remove_from_set, Set, Item} , riak_sets_vnode_master).

remove_from_set(Set) ->
    DocIdx               = riak_core_util:chash_key({<<"set">>,term_to_binary(Set)}),
    PrefList             = riak_core_apl:get_primary_apl(DocIdx, 1, riak_sets),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, {remove_from_set, Set} , riak_sets_vnode_master).

    
size(Set) ->
    DocIdx               = riak_core_util:chash_key({Set}),
    PrefList             = riak_core_apl:get_primary_apl(DocIdx, 1, riak_sets),

    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, {size, Set} , riak_sets_vnode_master).

