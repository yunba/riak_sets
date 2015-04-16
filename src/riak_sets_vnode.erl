-module(riak_sets_vnode).
-behaviour(riak_core_vnode).
-include("riak_sets.hrl").
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
    lager:info("PING ~p  ~p", [_Sender, State]),
    {reply, {pong, State#state.partition}, State};

handle_command({add_to_set, SetKey, Item}, _Sender, State = #state{data = Tree}) ->
               
    lager:info("add_to_set(~p,~p)", [SetKey, Item]), 
    case gb_trees:is_defined(SetKey, Tree) of
        false ->
            
            Set@                         = gb_sets:empty(),
            Set@                         = gb_sets:add(Item, Set@),
            Tree@                        = gb_trees:insert(SetKey, Set@, Tree),
            {reply, ok, State#state{data = Tree@}};
        true ->
            Set@                         = gb_trees:get(SetKey,  Tree),
            Set@                         = gb_sets:add(Item, Set@),
            Tree@                        = gb_trees:update(SetKey, Set@, Tree),
            {reply, ok, State#state{data = Tree@}}
    end;

handle_command({item_in_set, SetKey, Item}, _Sender, State = #state{data = Tree}) ->
    lager:info("item_in_set(~p,~p)", [SetKey, Item]),
    case gb_trees:lookup(SetKey, Tree) of
        none ->
            {reply, false, State};
        {value, SetData } ->
            
            lager:info("item_in_set SetData: ~p", [SetData]),
            {reply, gb_sets:is_member(Item, SetData), State}
    end;

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


handle_command(Message, _Sender, State) ->
    lager:warning("Unknown Command ~p",[{unhandled_command, Message}]),
    {reply,false, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

is_empty(State) ->
    {true, State}.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
