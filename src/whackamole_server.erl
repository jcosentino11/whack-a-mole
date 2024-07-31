-module(whackamole_server).

-include("whackamole_constants.hrl").

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
    % TODO set frame size, idle timeout
    {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
    io:format("websocket initialized. state:~p~n", [State]),
    {[], #player{player_id = whackamole_util:uuid(), websocket_id = self()}, hibernate}.

websocket_handle({text, <<"ready">>} = Message, #player{} = State) ->
    % TODO proper debug/trace logging
    io:format("message received: ~p, state: ~p~n", [Message, State]),
    whackamole_manager:player_ready(State),
    {[], State, hibernate};
websocket_handle({ping, _}, State) ->
    {[], State, hibernate};
websocket_handle({pong, _}, State) ->
    {[], State, hibernate};
websocket_handle(_Data, State) ->
    io:format("unknown message received: ~p~n", [_Data]),
    {[], State, hibernate}.

websocket_info({game_started, _Game}, State) ->
    io:format("notifying game started. ws_pid: ~p~n", [self()]),
	% TODO return game as binary
    {[{text, <<"Game Started">>}], State};
websocket_info({game_over, _Game}, State) ->
	% TODO return game as binary
    {[{text, <<"Game Over">>}], State};
websocket_info(_Info, State) ->
    {[], State}.
