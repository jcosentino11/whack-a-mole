
-define(GAME_MANAGER, game_manager).

-define(GAME_DURATION_MILLIS, 5000).
-define(PLAYERS_PER_GAME, 2).
-define(BOARD_SIZE, 25). % 5 x 5

-record(player, {websocket_id, player_id, board}).
-record(game, {game_id, players, required_player_count, started, duration, board_size}).
