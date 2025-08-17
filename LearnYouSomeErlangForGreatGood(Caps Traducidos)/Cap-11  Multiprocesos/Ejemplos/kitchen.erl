-module(kitchen).
-compile(export_all).

fridge1() ->
	receive
		{From, {store, _Food}} ->
			From ! {self(), ok},
			fridge1();
		{From, {take, _Food}} ->
			%% uh....
			From ! {self(), not_found},
			fridge1();
		terminate ->
			ok
	end.

fridge2(FoodList) ->
    receive
      {From, {store, Food}} ->
        From ! {self(), ok},
        fridge2([Food | FoodList]);
      {From, {take, Food}} ->
        case lists:member(Food, FoodList) of
          true ->
            From ! {self(), ok},
            fridge2(lists:delete(Food, FoodList));
          false -> 
            From ! not_found,
            fridge2(FoodList)
        end;
      terminate ->
        ok
      after 3000 ->
        timeout
    end.