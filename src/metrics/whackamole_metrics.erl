-module(whackamole_metrics).

-export([init/0]).

-export([emit_ws_connected/0, emit_ws_disconnected/0]).
-export([emit_game_started/0, emit_game_over/0]).

-export([get_num_ws_connected/0]).
-export([get_num_active_games/0]).

-define(WEBSOCKET_CONN_COUNT, websocket_conn_count).
-define(ACTIVE_GAME_COUNT, active_game_found).

init() ->
    ets:new(metrics, [named_table, public, set]),
    ets:insert(metrics, {?WEBSOCKET_CONN_COUNT, 0}),
    ets:insert(metrics, {?ACTIVE_GAME_COUNT, 0}).

emit_ws_connected() ->
    inc_counter(?WEBSOCKET_CONN_COUNT).

emit_ws_disconnected() ->
    dec_counter(?WEBSOCKET_CONN_COUNT).

emit_game_started() ->
    inc_counter(?ACTIVE_GAME_COUNT).

emit_game_over() ->
    dec_counter(?ACTIVE_GAME_COUNT).

get_num_ws_connected() ->
    get_counter(?WEBSOCKET_CONN_COUNT).

get_num_active_games() ->
    get_counter(?ACTIVE_GAME_COUNT).

inc_counter(Name) ->
    ets:update_counter(metrics, Name, {2, 1}).

dec_counter(Name) ->
    ets:update_counter(metrics, Name, {2, -1}).

get_counter(Name) ->
    case ets:lookup(metrics, Name) of
        [{Name, Count}] ->
            Count;
        [] ->
            0
    end.
