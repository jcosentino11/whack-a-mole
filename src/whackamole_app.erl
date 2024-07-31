-module(whackamole_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
		{'_', [
            {"/", cowboy_static, {priv_file, whackamole, "index.html"}},
            {"/static/[...]", cowboy_static, {priv_dir, whackamole, "static"}},
			{"/server", whackamole_server, []}
		]}
	]),
    whackamole_manager:spawn_game_manager(),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
		env => #{dispatch => Dispatch}
	}),
    whackamole_sup:start_link().

stop(_State) ->
    ok.
