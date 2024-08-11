-module(whackamole_app).

-behaviour(application).

-export([start/2, stop/1]).

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
    whackamole_bot:spawn_bot(),
    {ok, self()}.

stop(_State) ->
    ok.
