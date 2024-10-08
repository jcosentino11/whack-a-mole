-module(whackamole_config).

-export([
    port/0,
    app_env/0,
    game_duration_millis/0,
    players_per_game/0,
    board_size/0,
    board_update_interval_millis/0,
    max_players_allowed/0,
    ws_idle_timeout_millis/0,
    ws_max_frame_size/0
]).

port() ->
    getenv_int("PORT", 8080).

app_env() ->
    os:getenv("APP_ENV", "dev").

game_duration_millis() ->
    getenv_int("GAME_DURATION_MILLIS", 20000).

players_per_game() ->
    getenv_int("PLAYERS_PER_GAME", 10).

board_size() ->
    getenv_int("BOARD_SIZE", 25).

board_update_interval_millis() ->
    getenv_int("BOARD_UPDATE_INTERVAL_MILLIS", 3000).

max_players_allowed() ->
    getenv_int("MAX_PLAYERS_ALLOWED", 1000).

ws_idle_timeout_millis() ->
    getenv_int("WS_IDLE_TIMEOUT_MILLIS", 5000).

ws_max_frame_size() ->
    getenv_int("WS_MAX_FRAME_SIZE", (500 + players_per_game() * 2 * board_size())).

getenv_int(Name, DefaultValue) ->
    Val = os:getenv(Name, DefaultValue),
    case string:to_integer(Val) of
        {error, _} -> DefaultValue;
        {IntVal, _Rest} -> 
            IntVal
    end.
