-module(whackamole_game).

-include("whackamole_constants.hrl").

-export([spawn_game/0, game/1]).

spawn_game() ->
    GameId = whackamole_util:uuid(),
    InitialState =
        InitialState = #game{
            game_id = GameId,
            players = [],
            duration = ?GAME_DURATION_MILLIS,
            required_player_count = ?PLAYERS_PER_GAME,
            board_size = ?BOARD_SIZE
        },
    spawn(?MODULE, game, [InitialState]).

game(#game{duration = Duration, players = Players} = GameState) ->
    receive
        stop ->
            ok;
        {add_player, Player, CallerPid} ->
            case add_player(Player, GameState) of
                full ->
                    CallerPid ! full,
                    game(GameState);
                {GameStatus, UpdatedGameState} ->
                    CallerPid ! GameStatus,
                    game(UpdatedGameState)
            end;
        start_game ->
            notify_players(Players, {game_started, GameState}),
            erlang:send_after(Duration, self(), game_over),
            game(GameState);
        game_over ->
            notify_players(Players, {game_over, GameState});
        {update, _PlayerId, _MoleHit} ->
            % TODO ignore if game hasn't started yet
            % TODO update internal state
            game(GameState)
    end.

add_player(
    _Player,
    #game{players = Players, required_player_count = RequiredPlayerCount}
) when length(Players) >= RequiredPlayerCount ->
    full;
add_player(
    #player{websocket_id = WebsocketId, player_id = PlayerId},
    #game{players = Players, required_player_count = RequiredPlayerCount, board_size = BoardSize} =
        Game
) ->
    UpdatedPlayers =
        Players ++
            [
                #player{
                    websocket_id = WebsocketId, player_id = PlayerId, board = game_board(BoardSize)
                }
            ],
    UpdatedGame = Game#game{players = UpdatedPlayers},
    case length(UpdatedPlayers) of
        RequiredPlayerCount ->
            {ready, UpdatedGame};
        _ ->
            {pending, UpdatedGame}
    end.

game_board(Size) ->
    [0 || _ <- lists:seq(1, Size)].

notify_players([], _Message) ->
    ok;
notify_players([#player{websocket_id = WebsocketId} | Rest] = _Players, Message) ->
    WebsocketId ! Message,
    notify_players(Rest, Message).
