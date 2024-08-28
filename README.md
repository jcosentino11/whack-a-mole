# Whack-a-Mole

An online multiplayer game of whack-a-mole.

[Demo Video](https://github.com/user-attachments/assets/a7e3faf7-b0d3-4c8e-ac06-47e9b143093b)

## Play it!

```
docker run --rm -it -p 8080:8080 jcosentino11/whack-a-mole:latest
```
then go to `http://localhost:8080/`.


## Configuration

Configuration is done via environment variables:

* `PORT`: The application port, defaults to `8080`.
* `APP_ENV`: The environment the application is deployed in, defaults to `dev`.
* `GAME_DURATION_MILLIS`: How long a game of whack-a-mole lasts, defaults to `10000`.
* `PLAYERS_PER_GAME`: How many players are part of a whack-a-mole game, defaults to `2`.
* `BOARD_SIZE`: How many moles to have in a board, defaults to `25`.
* `BOARD_UPDATE_INTERVAL_MILLIS`: How often the moles should move around, defaults to `1000`.
* `MAX_PLAYERS_ALLOWED`: How many players can be connected at once, defaults to `1000`.
* `WS_IDLE_TIMEOUT_MILLIS`: How long the websocket connection can remain idle before the server terminates it, defaults to `5000`.
* `WS_MAX_FRAME_SIZE`: How large a websocket frame is allowed to be, defaults to [this formula](https://github.com/jcosentino11/whack-a-mole/blob/28bf662dfb84906f27b9d080009d52ceee0e2179/src/whackamole_config.erl#L32).

## Future Work

* Better matchmaking (e.g. create your own lobby, set game options)
* Make the UI look good, esp. for mobile
