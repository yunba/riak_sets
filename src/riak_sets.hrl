-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-compile( {parse_transform, lager_transform}).


-type setkey()   :: {setkey,binary()}.
-type setvalue() :: {setvalue,binary()}.

-type settree()  :: gb_trees:tree(setkey(), setvalue()).
