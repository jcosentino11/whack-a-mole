-module(whackamole_server).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

% game creator process
-define(GAME_CREATION_LOOP_DELAY_MILLIS, 1000).
-define(GAME_MANAGER, game_manager).
-export([spawn_game_manager/0, game_manager/1]).
-export([spawn_game/0, game/1]).

-define(PLAYERS_PER_GAME, 1).
-define(BOARD_SIZE, 5).
-define(GAME_DURATION_MILLIS, 60000).

-record(player, {websocket_id, player_id, board}).
-record(game, {game_id, players, required_player_count, started, duration}).

% =================================
% Websocket Handlers
% ---------------------------------

init(Req, Opts) ->
    spawn_game_manager(),
    % TODO set frame size, idle timeout
    {cowboy_websocket, Req, Opts}.

websocket_init(_State) ->
    {[], #player{player_id = uuid(), websocket_id = self()}}.

websocket_handle({text, "ready"}, #player{} = State) ->
    ?GAME_MANAGER ! {player_ready, State},
    {[], State};
websocket_handle(_Data, State) ->
    {[], State}.

websocket_info({start_game, _Game}, State) ->
    % TODO figure out resp format
    {reply, {binary, <<"Game started!">>}, State};
websocket_info(_Info, State) ->
    {[], State}.

% =================================
% Game Manager Process
% ---------------------------------

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
                    game_manager([GamePids | GamePid])
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
                            game_manager([GamePids | GamePid2])
                    end;
                ready ->
					GamePid ! start_game,
                    game_manager(GamePids);
                _ ->
                    game_manager(GamePids)
            end
    end.

% TODO
reap(GamePids) ->
    ok.

new_game(Player) ->
    GamePid = spawn_game(),
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

% =================================
% Game Process
% ---------------------------------

spawn_game() ->
    GameId = uuid(),
    InitialState =
        InitialState = #game{
            game_id = GameId,
            players = [],
            duration = ?GAME_DURATION_MILLIS,
            required_player_count = ?PLAYERS_PER_GAME
        },
    spawn(?MODULE, game, [InitialState]).

game(#game{duration = Duration} = GameState) ->
    Timer = erlang:send_after(Duration, self(), game_over),
    receive
        stop ->
            erlang:cancel_timer(Timer),
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
        {start_game} ->
            % TODO notify all players
            ok;
        game_over ->
            % TODO notify players of final state
            ok;
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
    #game{players = Players, required_player_count = RequiredPlayerCount} = Game
) ->
    UpdatedPlayers = [
        Players
        | #player{
            websocket_id = WebsocketId, player_id = PlayerId, board = game_board()
        }
    ],
    UpdatedGame = Game#game{players = UpdatedPlayers},
    case length(UpdatedPlayers) of
        RequiredPlayerCount ->
            {ready, UpdatedGame};
        _ ->
            {pending, UpdatedGame}
    end.

game_board() ->
    % TODO dynamically generate
    {{0, 0, 0, 0, 0}, {0, 0, 0, 0, 0}, {0, 0, 0, 0, 0}, {0, 0, 0, 0, 0}, {0, 0, 0, 0, 0}}.

% =================================
% Utilities
% ---------------------------------

% TODO refactor duplicate
-spec uuid() -> binary().
uuid() ->
    Id = erlang:make_ref(),
    Id1 = erlang:ref_to_list(Id),
    Id2 = erlang:list_to_binary(Id1),
    Id2.
