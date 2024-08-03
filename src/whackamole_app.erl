-module(whackamole_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    whackamole_metrics:init(),
    whackamole_manager:spawn_game_manager(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, whackamole, "index.html"}},
            {"/static/[...]", whackamole_template, {priv_dir, whackamole, "templates"}},
            {"/icons/[...]", cowboy_static, {priv_dir, whackamole, "icons"}},
            {"/info", whackamole_info_handler, []},
            {"/server", whackamole_server, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),
    whackamole_sup:start_link().

stop(_State) ->
    ok.
