-module(whackamole_server).

-include("whackamole_constants.hrl").

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-record(ws_state, {websocket_id, player_id, game_id}).

-define(MAX_FRAME_SIZE, 500 + (?PLAYERS_PER_GAME * 2 * ?BOARD_SIZE)).
% effectively used to close the ws connection if not enough players are found for a game
-define(IDLE_TIMEOUT_MILLIS, 5000). 

init(Req, State) ->
    {cowboy_websocket, Req, State, #{
        max_frame_size => ?MAX_FRAME_SIZE, idle_timeout => ?IDLE_TIMEOUT_MILLIS
    }}.

websocket_init(_State) ->
    {[], #ws_state{websocket_id = self()}, hibernate}.

websocket_handle({text, <<"ready">>}, #ws_state{websocket_id = WebsocketId} = State) ->
    whackamole_manager:player_ready(#player{websocket_id = WebsocketId}),
    {[], State, hibernate};
websocket_handle(
    {text, <<"hit", Index/binary>>}, #ws_state{game_id = GameId, player_id = PlayerId} = State
) ->
    whackamole_manager:hit(GameId, PlayerId, list_to_integer(binary:bin_to_list(Index))),
    {[], State};
websocket_handle(_Data, State) ->
    {[], State, hibernate}.

websocket_info({player_id, PlayerId}, State) ->
    UpdatedState = State#ws_state{player_id = PlayerId},
    {[], UpdatedState};
websocket_info({game_id, GameId}, State) ->
    UpdatedState = State#ws_state{game_id = GameId},
    {[], UpdatedState};
websocket_info(#game{} = Game, #ws_state{player_id = PlayerId} = State) ->
    {[{text, encode(Game, PlayerId)}], State};
websocket_info(_Info, State) ->
    {[], State}.

% TODO remove player on disconnect if game hasn't started

encode(Game, PlayerId) ->
    CurrPlayer = [
        Player
     || #player{player_id = _PlayerId} = Player <- Game#game.players, PlayerId =:= _PlayerId
    ],
    OtherPlayers = [
        Player
     || #player{player_id = _PlayerId} = Player <- Game#game.players, PlayerId /= _PlayerId
    ],
    Resp = #{
        game_id => erlang:list_to_binary(erlang:pid_to_list(Game#game.game_id)),
        player => lists:map(
            fun(Player) ->
                #{
                    player_id => Player#player.player_id,
                    score => Player#player.score,
                    board => Player#player.board
                }
            end,
            CurrPlayer
        ),
        players => lists:map(
            fun(Player) ->
                #{
                    player_id => Player#player.player_id,
                    score => Player#player.score,
                    board => Player#player.board
                }
            end,
            OtherPlayers
        ),
        required_player_count => Game#game.required_player_count,
        state => Game#game.state,
        duration => Game#game.duration,
        board_size => Game#game.board_size
    },
    jiffy:encode(Resp).
