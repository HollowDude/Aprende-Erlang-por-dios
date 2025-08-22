-module(plus).
-compile(export_all).
%-export([start/1, send_normal/2, send_secret/2, chisme/1]).
-record(secret, {from, msg}).

start(Secrets) ->
    spawn(?MODULE, confesor, [Secrets]).

confesor(Secrets) ->
    receive
        {From, {normal, Text}} ->
            From ! {ok, Text},
            confesor(Secrets);
        {From, {secret, Text}} ->
            confesor([#secret{from = From, msg = Text}|Secrets]);
        {From, chismes} ->
            From ! {ok, Secrets},
            confesor(Secrets)
    end.

send_normal(Pid, Msg) ->
    Pid ! {self(), {normal, Msg}},
    receive
        {ok, Reply} ->
            Reply
    after 2000 ->
        timeout
    end.

send_secret(Pid, Msg) -> Pid ! {self(), {secret, Msg}}.

chisme(Pid) -> 
    Pid ! {self(), chismes},
    receive
        {ok, Chismes} ->
            Chismes
    after 2000 ->
        timeout
    end.
