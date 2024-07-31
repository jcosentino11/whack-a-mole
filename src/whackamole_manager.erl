-module(whackamole_manager).

-include("whackamole_constants.hrl").

-export([player_ready/1]).
-export([spawn_game_manager/0, game_manager/1]).

player_ready(#player{websocket_id = _} = Player) ->
    ?GAME_MANAGER ! {player_ready, Player}.

spawn_game_manager() ->
    Pid = spawn(?MODULE, game_manager, [[]]),
    register(?GAME_MANAGER, Pid).

game_manager(GamePids) ->
    receive
        {player_ready, #player{} = Player} ->
            UpdatedGamePids = add_player(Player, GamePids),
            start_games(UpdatedGamePids),
            game_manager(UpdatedGamePids)
    end.

add_player(#player{} = Player, GamePid) when is_pid(GamePid) ->
    case is_process_alive(GamePid) of
        true ->
            GamePid ! {add_player, Player, self()},
            receive % TODO tiemout
                full ->
                    GamePid2 = whackamole_game:spawn_game(),
                    add_player(Player, GamePid2) ++ [GamePid];
                _ ->
                    [GamePid]
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
            receive % TODO timeout
                ok ->
                    start_games(Rest);
                error ->
                    ok
            end
    end.

% TODO
% reap(GamePids) ->
%     ok.
