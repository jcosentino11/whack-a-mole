
-define(GAME_MANAGER, game_manager).

-define(GAME_DURATION_MILLIS, 5000).
-define(PLAYERS_PER_GAME, 2).
-define(BOARD_SIZE, 25). % 5 x 5
-define(BOARD_UPDATE_INTERVAL_MILLIS, 1000).

-record(player, {websocket_id, player_id, board, score}).
-record(game, {game_id, state, players, required_player_count, duration, board_update_interval_millis, board_size}).
