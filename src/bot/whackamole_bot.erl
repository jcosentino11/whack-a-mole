-module(whackamole_bot).

-include("./../whackamole.hrl").

-export([spawn_bot/0, bot/0, send_ready/2]).

spawn_bot() ->
    spawn(?MODULE, bot, []).

bot() ->
    ConnTimeout = 1000,
    case connect(ConnTimeout) of
        {ok, ConnPid, StreamRef} ->
            send_ready(ConnPid, StreamRef),
            Res = recv(ConnPid, StreamRef),
            io:format("bot exited. reason: ~p~n", Res);
        Err ->
            io:format("bot creation failed: ~p~n", [Err]),
            Err
    end.

connect(TimeoutMillis) ->
    case gun:open("localhost", whackamole_config:port()) of
        {ok, ConnPid} ->
            case gun:await_up(ConnPid) of
                {ok, _} ->
                    gun:ws_upgrade(ConnPid, "/server"),
                    receive
                        {gun_upgrade, _ConnPid, StreamRef, [<<"websocket">>], _Headers} ->
                            {ok, ConnPid, StreamRef};
                        {gun_response, _ConnPid, _, _, _Status, _Headers} ->
                            {error, no_upgrade};
                        {gun_error, _ConnPid, _StreamRef, Reason} ->
                            {error, Reason}
                    after TimeoutMillis ->
                        {error, timed_out}
                    end;
                _ ->
                    {error, conn_failed}
            end;
        _ ->
            {error, bind_failed}
    end.

send_ready(ConnPid, StreamRef) ->
    gun:ws_send(ConnPid, StreamRef, {text, <<"ready">>}),
    io:format("bot ready. conn: ~p~n", [ConnPid]).

recv(
    #{
        <<"type">> := <<"info">>
    },
    ConnPid,
    StreamRef
) ->
    % keep the connection alive
    gun:ws_send(ConnPid, StreamRef, {text, <<"ping">>}),
    recv(ConnPid, StreamRef);
recv(
    #{
        <<"type">> := <<"game">>,
        <<"body">> := #{
            <<"state">> := <<"over">>
        }
    },
    ConnPid,
    StreamRef
) ->
    % TODO jitter is workaround to likely race condition when sending ready messages all at once
    timer:apply_after(500 + rand:uniform(5000), whackamole_bot, send_ready, [ConnPid, StreamRef]),
    recv(ConnPid, StreamRef);
recv(_Resp, ConnPid, StreamRef) ->
    recv(ConnPid, StreamRef).
recv(ConnPid, StreamRef) ->
    receive
        {gun_ws, _ConnPid, _StreamRef, {text, Payload}} ->
            recv(jiffy:decode(Payload, [return_maps]), ConnPid, StreamRef);
        % TODO handle disconnects
        _ ->
            recv(ConnPid, StreamRef)
    after 5000 ->
        timed_out
    end.
