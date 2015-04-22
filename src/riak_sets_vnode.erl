-module(riak_sets_vnode).
-behaviour(riak_core_vnode).
-include("riak_sets.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").
-compile({parse_transform,seqbind}).


-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).
-compile(export_all).
-export([merge_trees/2, set_size/3]).
-ignore_xref([
             start_vnode/1
             ]).

-record(state, {partition, 
                data :: settree()}).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    {ok, #state { partition = Partition,
                  data      = gb_trees:empty()
                }}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sample command: respond to a ping
handle_command(ping, _Sender, State) ->
    lager:debug("PING ~p  ~p", [_Sender, State]),
    {reply, {pong, State#state.partition}, State};

handle_command({ReqId, Cmd }, _Sender, State) when is_integer(ReqId) ->
    {Status, Resp, NewState} = handle_command(Cmd, _Sender, State),
    {Status, {ReqId, Resp}, NewState};
	
handle_command({add_to_set, SetKey, Item}, _Sender, State = #state{data = Tree}) ->
    lager:debug("add_to_set(~p,~p)", [SetKey, Item]), 
    add_to_set_tree(SetKey, Item, State, Tree);

handle_command({item_in_set, SetKey, Item}, _Sender, State = #state{data = Tree}) ->
    lager:debug("item_in_set(~p,~p)", [SetKey, Item]),
    get_item_from_set(SetKey, Item, State, Tree);

handle_command({remove_from_set, SetKey}, _Sender, State =  #state{data = Tree}) ->
    case gb_trees:is_defined(SetKey, Tree) of
        true ->
            {reply, ok, State#state{data = gb_trees:delete(SetKey, Tree)}};
        false ->
            {reply, ok, State}
        end;

handle_command({remove_from_set, SetKey, Item}, _Sender, State =  #state{data = Tree}) ->
    case gb_trees:lookup(SetKey, Tree) of
        {value, Set} ->
            NewSet  = gb_sets:del_element(Item, Set),
            NewTree = gb_trees:update(SetKey, NewSet, Tree), 
            {reply, ok, State#state{data = NewTree}};
        none ->
            {reply, ok, State}
    end;

handle_command({size, SetKey},_Sender, State =  #state{data = Tree}) ->
    set_size(SetKey, State, Tree);

handle_command({get_tree, _}, _,  State =  #state{data = Tree}) ->
    {reply, Tree, State};

handle_command(Message, _Sender, State) ->
    lager:warning("Unknown Command ~p",[ Message]),
    {reply,false, State}.



handle_handoff_command(?FOLD_REQ{foldfun=_VisitFun, acc0=_Acc0}, _Sender, State  = #state{data = Tree}) ->

    {reply, Tree, State};
handle_handoff_command(Message, _Sender, State) ->
    ?PRINT({unhandled_handoff_command, Message}),
    {noreply, State}.


handoff_starting(_TargetNode, State) ->
    lager:notice("handle_starting(~p,~p)", [_TargetNode, State]),
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(Data, State =  #state{data = OldTree}) ->
    NewTree = binary_to_term(Data),
    {reply, ok, State#state{data= merge_trees(OldTree, NewTree)}}.


encode_handoff_item(_ObjectName, ObjectValue) ->
    term_to_binary(ObjectValue).


is_empty(State =  #state{data = Tree}) ->
    case gb_trees:size(Tree) of
	0 ->
	    {true, State};
	_Size ->
	    false		
    end.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% Private API

%% get_all_objects(_State  = #state{data = Tree}) ->
%%     gb_trees:keys(Tree).

-spec(add_to_set_tree(term(), term(), #state{}, term()) -> {reply, ok, #state{}}).
add_to_set_tree(SetKey, Item, State, Tree) ->
    case gb_trees:is_defined(SetKey, Tree) of
        false ->
            lager:debug("No Set for key ~p", [SetKey]),
            Set@                         = gb_sets:empty(),
            Set@                         = gb_sets:add(Item, Set@),
            Tree@                        = gb_trees:insert(SetKey, Set@, Tree),
            {reply, ok, State#state{data = Tree@}};
        true ->
            Set@                         = gb_trees:get(SetKey,  Tree),
            lager:debug("set members ~p", [gb_sets:to_list(Set@)]),
            Set@                         = gb_sets:add(Item, Set@),
            Tree@                        = gb_trees:update(SetKey, Set@, Tree),
            {reply, ok, State#state{data = Tree@}}
    end.


merge_trees(Tree1, Tree2) ->
    lists:foldr(fun({SetKey, Value}, Tr) ->
                        union_to_set_tree(SetKey, Value, Tr)
                end, Tree1, gb_trees:to_list(Tree2)).

    

union_to_set_tree(SetKey, NewSet, Tree) ->
    case gb_trees:is_defined(SetKey, Tree) of
        false ->
            gb_trees:insert(SetKey, NewSet, Tree);
        true ->
            Set@                         = gb_trees:get(SetKey,  Tree),
            Set@                         = gb_sets:union(NewSet, Set@),
            Tree@                        = gb_trees:update(SetKey, Set@, Tree),
            Tree
    end.

get_item_from_set(SetKey, Item, State, Tree) ->
    case gb_trees:lookup(SetKey, Tree) of
        none ->
            {reply, false, State};
        {value, SetData } ->
            lager:debug("item_in_set SetData: ~p", [SetData]),
            {reply, gb_sets:is_member(Item, SetData), State}
    end.

set_size(SetKey, State, Tree) ->
    case gb_trees:lookup(SetKey, Tree) of
        {value, Set} ->
            {reply, gb_sets:size(Set), State};
        none ->
            lager:debug("Set ~p Not Found" , [SetKey]),
            {reply, 0, State}
    end.
