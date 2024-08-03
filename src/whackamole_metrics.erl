-module(whackamole_metrics).

-export([init/0]).

-export([emit_ws_connected/0, emit_ws_disconnected/0]).
-export([get_num_ws_connected/0]).

init() ->
    ets:new(metrics, [named_table, public, set]),
    ets:insert(metrics, {websocket_conn_count, 0}),
    ets:insert(metrics, {games_count, 0}).

emit_ws_connected() ->
    ets:update_counter(metrics, websocket_conn_count, {2, 1}).

emit_ws_disconnected() ->
    ets:update_counter(metrics, websocket_conn_count, {2, -1}).

emit_game_started() ->
    ets:update_counter(metrics, games_count, {2, 1}).

emit_game_over() ->
    ets:update_counter(metrics, games_count, {2, -1}).

get_num_ws_connected() ->
    get_counter(games_count).

get_num_active_games() ->
    get_counter(games_count).

get_counter(Name) ->
    case ets:lookup(metrics, Name) of
        [{Name, Count}] ->
            Count;
        [] ->
            0
    end.
