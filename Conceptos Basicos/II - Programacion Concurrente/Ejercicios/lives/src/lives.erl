-module(lives).
-export([start/1, ganar/1, perder/1, estado/0, veneno/0]).

-include("lives_common.hrl").

veneno() ->
    lives_server:send(?MSG_POISON).

estado() ->
    lives_server:send(?MSG_STAT).

ganar(N) ->
    lives_server:send({?MSG_GAIN, N}).

perder(N) ->
    lives_server:send({?MSG_LOSE, N}).

start(N) ->
    lives_server:start(N).

