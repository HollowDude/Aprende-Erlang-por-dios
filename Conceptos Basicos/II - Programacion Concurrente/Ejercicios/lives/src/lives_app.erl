-module(lives_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    {ok, Lives} = application:get_env(initial_lives),
    io:format("Iniciando la app de mierda esta ~n", []),
    lives:start(Lives),
    {ok, self()}.

stop(_State) ->
    io:format("Cerrando la app de mierda esta ~n", []),
    ok.