const ClientGameState = Object.freeze({
  // no connection to server
  DISCONNECTED: "DISCONNECTED",
  // connected and ready to play a game
  READY: "READY",
  // searching for a game and players
  SEARCHING: "SEARCHING",
  // game in progress
  INPROGRESS: "INPROGRESS",
  // game is over
  OVER: "OVER",
});

class State {
  static Properties = Object.freeze({
    CLIENT_GAME_STATE: {
      name: "CLIENT_GAME_STATE",
      default: ClientGameState.DISCONNECTED,
    },
    BOARD: { name: "BOARD", default: [] },
    SCORE: { name: "SCORE", default: 0 },
    PLAYERS: { name: "PLAYERS", default: [] },
    ACTIVE_GAME_COUNT: { name: "ACTIVE_GAME_COUNT", default: 0 },
    WEBSOCKET_CONN_COUNT: { name: "WEBSOCKET_CONN_COUNT", default: 0 },
  });

  constructor() {
    this.onChangeHandlers = {};
    this.#initialize();
  }

  #initialize() {
    Object.values(State.Properties).forEach(
      ({ name, default: defaultValue }) => {
        this.setProperty(name, defaultValue);
      }
    );
  }

  resetProperty(name) {
    Object.values(State.Properties).forEach(
      ({ propName, default: defaultValue }) => {
        if (propName == name) {
          this.setProperty(name, defaultValue);
        }
      }
    );
  }

  setProperty(name, val) {
    const prev = this[name];
    this[name] = val;
    this.#fireOnChange(name, prev, val);
  }

  getProperty(name) {
    return this[name];
  }

  registerOnChange(name, func) {
    if (!this.onChangeHandlers[name]) {
      this.onChangeHandlers[name] = [];
    }
    this.onChangeHandlers[name].push(func);
  }

  #fireOnChange(prop, prevVal, newVal) {
    if (prevVal != newVal) {
      const handlers = this.onChangeHandlers[prop];
      if (handlers) {
        handlers.forEach((handler) => {
          handler(prevVal, newVal);
        });
      }
    }
  }
}

class Renderer {
  constructor(state) {
    this.state = state;
    this.canvas = document.getElementById("gameCanvas");
    this.ctx = this.canvas.getContext("2d");
    this.cellSize = 100;
    this.activeGameCountElement = document.getElementById("activeGameCount");
    this.websocketConnCountElement =
      document.getElementById("websocketConnCount");
    this.startButton = document.getElementById("startButton");
    this.playerScore = document.getElementById("playerScore");
    this.winner = document.getElementById("winner");

    // wire rendering to state changes.
    // only parts that have changed will be rerendered.

    this.state.registerOnChange(State.Properties.CLIENT_GAME_STATE.name, () => {
      this.renderStartButton();
      this.renderInfo();
      this.renderWinner();
    });
    this.state.registerOnChange(State.Properties.PLAYERS.name, () => {
      this.renderWinner();
    });
    this.state.registerOnChange(State.Properties.BOARD.name, () => {
      this.renderBoard();
    });
    this.state.registerOnChange(
      State.Properties.WEBSOCKET_CONN_COUNT.name,
      () => this.renderInfo()
    );
    this.state.registerOnChange(State.Properties.ACTIVE_GAME_COUNT.name, () =>
      this.renderInfo()
    );
    this.state.registerOnChange(State.Properties.SCORE.name, () =>
      this.renderScore()
    );
  }

  registerOnStartGamePressed(func) {
    this.#registerOnClick(this.startButton, (_event) => {
      const clientGameState = this.state.getProperty(
        State.Properties.CLIENT_GAME_STATE.name
      );
      if (
        clientGameState == ClientGameState.OVER ||
        clientGameState == ClientGameState.READY
      ) {
        func();
      }
    });
  }

  registerOnMoleClick(func) {
    this.#registerOnClick(this.canvas, (event) => {
      const rect = this.canvas.getBoundingClientRect();
      const x = event.clientX - rect.left;
      const y = event.clientY - rect.top;
      const mole = this.#moleAt(x, y);
      if (
        mole != -1 &&
        this.state.getProperty(State.Properties.CLIENT_GAME_STATE.name) ==
          ClientGameState.INPROGRESS
      ) {
        func(mole);
      }
    });
  }

  #registerOnClick(ele, func) {
    ele.addEventListener("click", func);
  }

  renderInfo() {
    if (
      this.state.getProperty(State.Properties.CLIENT_GAME_STATE.name) ==
      ClientGameState.DISCONNECTED
    ) {
      this.activeGameCountElement.textContent = "unknown";
      this.websocketConnCountElement.textContent = "unknown";
    } else {
      this.activeGameCountElement.textContent = this.state.getProperty(
        State.Properties.ACTIVE_GAME_COUNT.name
      );
      this.websocketConnCountElement.textContent = this.state.getProperty(
        State.Properties.WEBSOCKET_CONN_COUNT.name
      );
    }
  }

  renderBoard() {
    this.ctx.clearRect(0, 0, this.canvas.width, this.canvas.height);
    const board = this.state.getProperty(State.Properties.BOARD.name);
    if (!board) return;
    for (let i = 0; i < board.length; i++) {
      const x = (i % 5) * this.cellSize;
      const y = Math.floor(i / 5) * this.cellSize;
      this.ctx.fillStyle = board[i] === 1 ? "red" : "white";
      this.ctx.fillRect(x, y, this.cellSize, this.cellSize);
      this.ctx.strokeRect(x, y, this.cellSize, this.cellSize);
    }
  }

  renderStartButton() {
    const clientGameState = this.state.getProperty(
      State.Properties.CLIENT_GAME_STATE.name
    );

    let { disabled, text } = (() => {
      switch (clientGameState) {
        case ClientGameState.DISCONNECTED:
          return { disabled: true, text: "Waiting for connection..." };
        case ClientGameState.INPROGRESS:
          return { disabled: true, text: "Playing..." };
        case ClientGameState.OVER:
          return { disabled: false, text: "Play Again" };
        case ClientGameState.SEARCHING:
          return { disabled: true, text: "Searching for players" };
        case ClientGameState.READY:
          return { disabled: false, text: "Ready!" };
        default:
          return { disabled: true, text: "..." };
      }
    })();

    this.startButton.disabled = disabled;
    this.startButton.textContent = text;
  }

  renderScore() {
    this.playerScore.textContent = this.state.getProperty(
      State.Properties.SCORE.name
    );
  }

  renderWinner() {
    const clientGameState = this.state.getProperty(
      State.Properties.CLIENT_GAME_STATE.name
    );

    const winnerText = () => {
      const players = this.state.getProperty(State.Properties.PLAYERS.name);
      if (players.length == 0) {
        return "WINNER!";
      }
      const bestOpponentScore = players.reduce(
        (maxScore, player) => Math.max(maxScore, player.score),
        players[0].score
      );
      const playerScore = this.state.getProperty(State.Properties.SCORE.name);
      if (playerScore == bestOpponentScore) {
        return "IT'S A TIE!";
      }
      if (playerScore > bestOpponentScore) {
        return "YOU WON!";
      }
      return "BETTER LUCK NEXT TIME...";
    };

    const text = (() => {
      switch (clientGameState) {
        case ClientGameState.OVER:
          return winnerText();
        case ClientGameState.SEARCHING:
          return "";
        default:
          null;
      }
    })();

    if (text !== null) {
      this.winner.textContent = text;
    }
  }

  #moleAt(x, y) {
    const i = Math.floor(x / this.cellSize) + Math.floor(y / this.cellSize) * 5;
    if (this.state.getProperty(State.Properties.BOARD.name)[i] == 1) {
      return i + 1; // moles are 1-indexed
    }
    return -1;
  }
}

class Connection {
  constructor(url) {
    this.url = url;
    this.ws = null;
    this.keepAliveIntervalId = null;
    this.onMessageHandlers = [];
    this.onOpenHandlers = [];
    this.onCloseHandlers = [];

    window.addEventListener("beforeunload", () => this.close());
  }

  registerOnMessage(func) {
    this.onMessageHandlers.push(func);
  }

  registerOnOpen(func) {
    this.onOpenHandlers.push(func);
  }

  registerOnClose(func) {
    this.onCloseHandlers.push(func);
  }

  connect() {
    if (
      this.ws &&
      [WebSocket.CLOSED, WebSocket.CLOSING].includes(this.ws.readyState)
    ) {
      this.close();
    }
    if (!this.ws) {
      this.ws = new WebSocket(this.url);
      this.ws.onmessage = (event) => {
        this.#invokeHandlers(this.onMessageHandlers, event.data);
      };
      this.ws.onopen = (_event) => {
        this.#invokeHandlers(this.onOpenHandlers);
      };
      this.ws.onclose = (_event) => {
        this.#invokeHandlers(this.onCloseHandlers);
        this.connect();
      };
      this.#keepAlive();
    }
  }

  send(message) {
    if (this.ws && this.ws.readyState === WebSocket.OPEN) {
      try {
        this.ws.send(message);
        return true;
      } catch (ignore) {}
    }
    return false;
  }

  close() {
    if (this.ws) {
      this.ws.close();
      this.ws = null;
      clearInterval(this.keepAliveIntervalId);
    }
  }

  #keepAlive() {
    clearInterval(this.keepAliveIntervalId);
    this.keepAliveIntervalId = setInterval(() => this.send("ping"), 1000); // TODO template
  }

  #invokeHandlers(handlers, arg) {
    handlers.forEach((handler) => {
      handler(arg);
    });
  }
}

class Game {
  constructor(state, connection, renderer) {
    this.state = state;
    this.connection = connection;
    this.renderer = renderer;

    this.state.registerOnChange(
      State.Properties.CLIENT_GAME_STATE.name,
      (prevVal, newVal) => this.#onClientGameStateChanged(prevVal, newVal)
    );

    this.connection.registerOnMessage((rawMessage) => {
      const message = JSON.parse(rawMessage);
      if (message.type == "game") {
        this.#onGameUpdateReceived(message.body);
      } else if (message.type == "info") {
        this.#onInfoUpdateReceived(message.body);
      }
    });

    this.connection.registerOnOpen(() => {
      this.state.setProperty(
        State.Properties.CLIENT_GAME_STATE.name,
        ClientGameState.READY
      );
    });
    this.connection.registerOnClose(() => {
      this.state.setProperty(
        State.Properties.CLIENT_GAME_STATE.name,
        ClientGameState.DISCONNECTED
      );
    });
    this.renderer.registerOnStartGamePressed(() => {
      state.setProperty(
        State.Properties.CLIENT_GAME_STATE.name,
        ClientGameState.SEARCHING
      );
    });

    this.renderer.registerOnMoleClick((moleId) => {
      this.connection.send(`hit${moleId}`);
    });
  }

  connect() {
    connection.connect();
  }

  #onClientGameStateChanged(_prevVal, newVal) {
    if (newVal == ClientGameState.SEARCHING) {
      // keep old state around so player can look at it, but when search starts, clear the slate.
      this.state.resetProperty(State.Properties.BOARD.name);
      this.state.resetProperty(State.Properties.SCORE.name);
      // notify server that client is ready to matchmake
      // once the game has started, server will respond with message containing game state
      this.connection.send("ready");
    }
  }

  #onGameUpdateReceived(game) {
    this.state.setProperty(State.Properties.BOARD.name, game.player[0].board);
    this.state.setProperty(State.Properties.SCORE.name, game.player[0].score);
    this.state.setProperty(State.Properties.PLAYERS.name, game.players);
    if (game.state == "over") {
      this.state.setProperty(
        State.Properties.CLIENT_GAME_STATE.name,
        ClientGameState.OVER
      );
    } else if (game.state == "started") {
      this.state.setProperty(
        State.Properties.CLIENT_GAME_STATE.name,
        ClientGameState.INPROGRESS
      );
    }
  }

  #onInfoUpdateReceived(info) {
    this.state.setProperty(
      State.Properties.ACTIVE_GAME_COUNT.name,
      info.active_game_count
    );
    this.state.setProperty(
      State.Properties.WEBSOCKET_CONN_COUNT.name,
      info.websocket_conn_count
    );
  }
}

const state = new State();
const connection = new Connection(
  "{{ws_proto}}://" + window.location.host + "/server"
);
const renderer = new Renderer(state);
const game = new Game(state, connection, renderer);
window.addEventListener("load", () => game.connect());
