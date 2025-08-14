-module(tut5).
-export([format/1]).

format([]) ->
    ok;

format([City | Rest]) ->
    print_temp(convert_celc(City)),
    format(Rest).

convert_celc({Name, {c, Temp}}) ->
    {Name, {c, Temp}};

convert_celc({Name, {f, Temp}}) ->
    {Name, {c, (Temp - 32) * 5 / 9}}.

print_temp({Name, {c, Temp}}) ->
    io:format("En celcious, la ciudad:~w~n Con esta temp:~w~n", [Name, Temp]).

