-module(whackamole_info_handler).

-behaviour(cowboy_handler).

-export([init/2, content_types_provided/2, info_json/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {
        [
            {<<"application/json">>, info_json}
        ],
        Req,
        State
    }.

info_json(Req, State) ->
    Resp = #{
        websocket_conn_count => whackamole_metrics:get_num_ws_connected(),
        active_game_count => whackamole_metrics:get_num_active_games()
    },
    Body = jiffy:encode(Resp),
    {Body, Req, State}.
