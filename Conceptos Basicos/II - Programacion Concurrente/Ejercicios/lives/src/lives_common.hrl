-define(TIMEOUT_NORMAL, 20000).
-define(TIMEOUT_POISON, 5000).

-define(MSG_GAIN, ganar).
-define(MSG_LOSE, perder).
-define(MSG_STAT, estado).
-define(MSG_POISON, poison).

-record(state, {
    lives, 
    poison = false
}).