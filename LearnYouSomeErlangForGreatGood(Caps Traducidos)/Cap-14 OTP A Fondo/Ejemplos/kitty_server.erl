%Version natica
-module(kitty_server).
-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).
-record(cat, {name, color=green, description}).

%Negocio muy legal de gatos, dicen que los naranjas te dan un viaje mas fuerte c;
%Client Api
start_link() ->
    spawn_link(fun init/0).

%Llammadas sincronas
order_cat(Pid, Name, Color, Description) ->
    Ref = monitor(process, Pid),
    Pid ! {self(), Ref, {order, Name, Color, Description}},
    receive
        {Ref, Cat} ->
            demonitor(Ref, [flush]),
            Cat;
        {'DOWN', Ref, process, Pid, Reason} ->
            error(Reason)
        after 5000 ->
            error(timeout)
    end.

%Esta es asincrona
return_cat(Pid, Cat = #cat{}) ->
    Pid ! {return, Cat},
    ok.

%Esta es sincrona
close_shop(Pid) ->
    Ref = monitor(process, Pid),
    Pid ! {self(), Ref, terminate},
    receive
        {Ref, ok} ->
            demonitor(Ref, [flush]),
            ok;
        {'DOWN', Ref, process, Pid, Reason} ->
            error(Reason)
        after 5000 ->
            error(timeout)
    end.

%Server function
init() -> loop([]).

loop(Cats) ->
    receive
        {Pid, Ref, {order, Name, Color, Description}} ->
            if
                Cats =:= [] ->
                    Pid ! {Ref, make_cat(Name, Color, Description)},
                    loop(Cats);
                cats =/= [] ->
                    Pid ! {Ref, hd(Cats)},
                    loop(tl(Cats))
            end;
        {return, Cat = #cat{}} ->
            loop([Cat|Cats]);
        {Pid, Ref, terminate} ->
            Pid ! {Ref, ok},
            terminate(Cats);
        Unknown ->
            %Ponemos sin mas unos logs comicones:
            io:format("Mensaje desconocido: ~p~n", [Unknown]),
            loop(Cats)
    end.

%Funciones privadas sin mas.
make_cat(Name, Color, Description) ->
    #cat{name=Name, color = Color, description = Description}.

terminate(Cats) ->
    [io:format("~p was set free.~n",[C#cat.name]) || C <- Cats],
	ok.
