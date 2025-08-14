-module(hhfunc).
%-compile(export_all). <------------- para produccion sobre todo
-export([add/2, num1/1, num2/1]).

% hhfunc:add(fun()-> hhfunc:num1(342) end,fun() -> hhfunc:num2(213) end). <---------- para llamarla

num1(X)-> X.
num2(X)-> X.
add(X, Y) ->
    X() + Y().