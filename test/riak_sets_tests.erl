
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


run_test_() ->
    application:ensure_all_started(lager),
    code:add_pathz("../apps/setref/ebin"),
    {timeout, 3600,
     ?_assertEqual([],proper:module(?MODULE,[100,{to_file, user}]))}.
