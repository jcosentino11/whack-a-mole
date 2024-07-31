-module(whackamole_manager).

-include("whackamole_constants.hrl").

-export([player_ready/1]).
-export([spawn_game_manager/0, game_manager/1]).

player_ready(#player{} = Player) ->
    ?GAME_MANAGER ! {player_ready, Player}.

spawn_game_manager() ->
    Pid = spawn(?MODULE, game_manager, [[]]),
    register(?GAME_MANAGER, Pid).

game_manager([] = GamePids) ->
    receive
        {player_ready, Player} ->
            case new_game(Player) of
                error ->
                    game_manager(GamePids);
                GamePid ->
                    game_manager(GamePids ++ [GamePid])
            end
    end;
game_manager(GamePids) ->
    receive
        {player_ready, Player} ->
            GamePid = lists:last(GamePids),
            GamePid ! {add_player, Player, self()},
            receive
                full ->
                    case new_game(Player) of
                        error ->
                            game_manager(GamePids);
                        GamePid2 ->
                            game_manager(GamePids ++ [GamePid2])
                    end;
                ready ->
                    GamePid ! start_game,
                    game_manager(GamePids);
                _ ->
                    game_manager(GamePids)
            end
    end.

new_game(Player) ->
    GamePid = whackamole_game:spawn_game(),
    GamePid ! {add_player, Player, self()},
    receive
        full ->
            % would only happen if players per game is 0
            GamePid ! stop,
            error;
        ready ->
            GamePid ! start_game,
            GamePid;
        _ ->
            GamePid
    end.

% TODO
% reap(GamePids) ->
%     ok.
