-module(whackamole_game_manager).

-include("./../whackamole.hrl").

-export([player_ready/1, hit/3, player_left/2]).
-export([spawn_game_manager/0, game_manager/1]).

player_ready(#player{websocket_id = _} = Player) ->
    send_message({player_ready, Player}).

hit(GameId, PlayerId, Index) ->
    send_message({hit, GameId, PlayerId, Index}).

player_left(GameId, PlayerId) ->
    send_message({player_left, GameId, PlayerId}).

send_message(Msg) ->
    case whereis(?GAME_MANAGER) of
        undefined -> manager_not_started;
        _ -> ?GAME_MANAGER ! Msg
    end.

spawn_game_manager() ->
    Pid = spawn(?MODULE, game_manager, [[]]),
    register(?GAME_MANAGER, Pid).

game_manager(GamePids) ->
    receive
        {player_ready, #player{} = Player} ->
            GamePids2 = cleanup_pids(GamePids),
            GamePids3 = add_player(Player, GamePids2),
            start_games(GamePids3),
            game_manager(GamePids3);
        {hit, GameId, PlayerId, Index} ->
            GameId ! {hit, PlayerId, Index},
            game_manager(GamePids);
        {player_left, GameId, PlayerId} ->
            GameId ! {player_left, PlayerId},
            game_manager(GamePids)
    end.

add_player(#player{} = Player, GamePid) when is_pid(GamePid) ->
    case is_process_alive(GamePid) of
        true ->
            GamePid ! {add_player, Player, self()},
            receive
                full ->
                    GamePid2 = whackamole_game:spawn_game(),
                    add_player(Player, GamePid2) ++ [GamePid];
                _ ->
                    [GamePid]
                % game process doesn't exist anymore
            after 1000 ->
                ok
            end;
        false ->
            GamePid2 = whackamole_game:spawn_game(),
            add_player(Player, GamePid2)
    end;
add_player(#player{} = Player, [] = _GamePids) ->
    GamePid = whackamole_game:spawn_game(),
    add_player(Player, GamePid);
add_player(#player{} = Player, [GamePid | Rest]) ->
    add_player(Player, GamePid) ++ Rest.

start_games([]) ->
    ok;
start_games([GamePid | Rest]) ->
    GamePid ! {start_game, self()},
    case is_process_alive(GamePid) of
        true ->
            receive
                ok ->
                    start_games(Rest);
                error ->
                    ok
                % game process doesn't exist anymore
            after 1000 ->
                ok
            end
    end.

cleanup_pids(GamePids) ->
    [GamePid || GamePid <- GamePids, is_process_alive(GamePid)].
