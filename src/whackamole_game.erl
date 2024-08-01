-module(whackamole_game).

-include("whackamole_constants.hrl").

-export([spawn_game/0, game/1]).

spawn_game() ->
    InitialState = #game{
        state = pending,
        players = [],
        duration = ?GAME_DURATION_MILLIS,
        required_player_count = ?PLAYERS_PER_GAME,
        board_size = ?BOARD_SIZE,
        board_update_interval_millis = ?BOARD_UPDATE_INTERVAL_MILLIS
    },
    spawn(?MODULE, game, [InitialState]).

game(
    #game{
        duration = Duration,
        board_update_interval_millis = UpdateInterval,
        players = Players,
        state = State
    } = GameState
) ->
    receive
        stop ->
            ok;
        {add_player, Player, CallerPid} ->
            case add_player(Player, GameState) of
                full ->
                    CallerPid ! full,
                    game(GameState);
                {#player{player_id = PlayerId} = UpdatedPlayer, UpdatedGameState} ->
                    notify_ws([UpdatedPlayer], {player_id, PlayerId}),
                    CallerPid ! ok,
                    game(UpdatedGameState)
            end;
        {start_game, CallerPid} ->
            case State of
                ready ->
                    GameId = self(),
                    UpdatedGameState = GameState#game{state = started, game_id = GameId},
                    notify_ws(Players, {game_id, GameId}),
                    notify_ws(Players, UpdatedGameState),
                    erlang:send_after(Duration, GameId, game_over),
                    erlang:send_after(UpdateInterval, GameId, next_board),
                    CallerPid ! ok,
                    game(UpdatedGameState);
                _ ->
                    CallerPid ! error,
                    game(GameState)
            end;
        game_over ->
            UpdatedGameState = GameState#game{state = over},
            notify_ws(Players, UpdatedGameState);
        next_board ->
            case State of
                started ->
                    erlang:send_after(UpdateInterval, self(), next_board),
                    UpdatedGameState = next_board(GameState),
                    notify_ws(Players, UpdatedGameState),
                    game(UpdatedGameState);
                _ ->
                    game(GameState)
            end
    end.

add_player(
    #player{} = _Player,
    #game{players = Players, required_player_count = RequiredPlayerCount}
) when length(Players) >= RequiredPlayerCount ->
    full;
add_player(
    #player{} = Player,
    #game{
        players = Players,
        required_player_count = RequiredPlayerCount,
        board_size = BoardSize,
        state = State
    } = Game
) ->
    PlayerId = length(Players) + 1,
    UpdatedPlayer = Player#player{player_id = PlayerId, board = game_board(BoardSize)},
    UpdatedPlayers = Players ++ [UpdatedPlayer],
    UpdatedState =
        case length(UpdatedPlayers) of
            RequiredPlayerCount ->
                ready;
            _ ->
                State
        end,
    UpdatedGame = Game#game{players = UpdatedPlayers, state = UpdatedState},
    {UpdatedPlayer, UpdatedGame}.

next_board(
    #game{
        players = Players
    } = Game
) ->
    UpdatedGame = Game#game{players = lists:map(fun next_board/1, Players)},
    UpdatedGame;
next_board(#player{board = PrevBoard} = Player) ->
    NewBoard = lists:map(fun(_) -> random_mole() end, PrevBoard),
    UpdatedPlayer = Player#player{board = NewBoard},
    UpdatedPlayer.

random_mole() ->
    rand:uniform(2) - 1.

game_board(Size) ->
    [0 || _ <- lists:seq(1, Size)].

notify_ws([], _Message) ->
    ok;
notify_ws([#player{websocket_id = WebsocketId} | Rest], Message) ->
    WebsocketId ! Message,
    notify_ws(Rest, Message).
