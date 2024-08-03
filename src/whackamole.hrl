-define(GAME_MANAGER, game_manager).

-record(player, {websocket_id, player_id, board, score}).
-record(game, {
    game_id,
    state,
    players,
    required_player_count,
    duration,
    board_update_interval_millis,
    board_size
}).
