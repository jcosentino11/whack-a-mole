-module(whackamole_config).

-export([
    app_env/0,
    game_duration_millis/0,
    players_per_game/0,
    board_size/0,
    board_update_interval_millis/0,
    ws_idle_timeout_millis/0,
    ws_max_frame_size/0
]).

app_env() ->
    os:getenv("APP_ENV", "dev").

game_duration_millis() ->
    os:getenv("GAME_DURATION_MILLIS", 10000).

players_per_game() ->
    os:getenv("PLAYERS_PER_GAME", 2).

board_size() ->
    os:getenv("BOARD_SIZE", 25).

board_update_interval_millis() ->
    os:getenv("BOARD_UPDATE_INTERVAL_MILLIS", 1000).

% effectively used to close the ws connection if not enough players are found for a game
ws_idle_timeout_millis() ->
    os:getenv("WS_IDLE_TIMEOUT_MILLIS", 5000).

ws_max_frame_size() ->
    os:getenv("WS_MAX_FRAME_SIZE", (500 + players_per_game() * 2 * board_size())).
