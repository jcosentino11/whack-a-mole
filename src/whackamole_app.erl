-module(whackamole_app).

-behaviour(application).

-export([start/2, stop/1, spawn_bots/1]).

start(_StartType, _StartArgs) ->
    whackamole_metrics:init(),
    whackamole_game_manager:spawn_game_manager(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, whackamole, "index.html"}},
            {"/static/[...]", whackamole_template, {priv_dir, whackamole, "templates"}},
            {"/icons/[...]", cowboy_static, {priv_dir, whackamole, "icons"}},
            {"/server", whackamole_game_server, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, whackamole_config:port()}], #{
        env => #{dispatch => Dispatch}
    }),
    % TODO delay is workaround to likely race condition when sending ready messages all at once
    timer:apply_after(500, whackamole_app, spawn_bots, [whackamole_config:players_per_game() - 1]),
    whackamole_sup:start_link().

spawn_bots(NumBots) when NumBots =< 0 ->
    ok;
spawn_bots(NumBots) ->
    whackamole_bot:spawn_bot(),
    timer:apply_after(500, whackamole_app, spawn_bots, [NumBots - 1]).

stop(_State) ->
    ok.
