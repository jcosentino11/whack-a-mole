-module(whackamole_template).

-export([init/2]).

template(Path) ->
    case file:read_file(Path) of
        {ok, Template} ->
            bbmustache:render(Template, template_args());
        Other ->
            {error, Other}
    end.

template_args() ->
    #{"ws_proto" => template_args_ws_proto()}.

template_args_ws_proto() ->
    case os:getenv("APP_ENV") of
        "prod" -> "wss";
        _ -> "ws"
    end.

content_type(Filepath) ->
    case filename:extension(Filepath) of
        <<".js">> -> <<"text/javascript">>;
        <<".html">> -> <<"text/html">>;
        _ -> <<"text/plain">>
    end.

init(Req, {priv_dir, Module, Path} = Opts) ->
    case cowboy_req:path_info(Req) of
        undefined ->
            {ok, cowboy_req:reply(500, Req), error};
        PathInfo ->
            Filepath = filename:join([code:priv_dir(Module), Path] ++ PathInfo),
            case template(Filepath) of
                {error, _} ->
                    {ok, cowboy_req:reply(500, Req), error};
                Template ->
                    ContentType = content_type(Filepath),
                    Reply = cowboy_req:reply(
                        200, #{<<"content-type">> => ContentType}, Template, Req
                    ),
                    {ok, Reply, Opts}
            end
    end.
