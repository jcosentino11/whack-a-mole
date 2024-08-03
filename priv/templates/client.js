const canvas = document.getElementById("gameCanvas");
const ctx = canvas.getContext("2d");
const cellSize = 100;

const infoEndpoint =
  window.location.protocol + "//" + window.location.host + "/info";
const activeGameCountElement = document.getElementById("activeGameCount");
const websocketConnCountElement = document.getElementById("websocketConnCount");
let playersPerGame = undefined;

const startButton = document.getElementById("startButton");
const startButtonDefaultText = "Ready!";
const playerScore = document.getElementById("playerScore");

let board;
let score = 0;

const server = "{{ws_proto}}://" + window.location.host + "/server";
let state;
let ws;

const startGame = () => {
  resetState();
  render();
  connect();
}

const resetState = () => {
  board = undefined;
  score = 0;
  state = undefined;
}

const connect = () => {
  if (ws) {
    ws.close();
  }
  ws = new WebSocket(server);
  ws.onmessage = (event) => {
    const gameState = JSON.parse(event.data);
    updateState(gameState);
    render();
  };
  ws.onopen = (_event) => {
    send("ready");
    enableStartButton(false);
  };
  ws.onclose = (_event) => {
    enableStartButton(true);
  };
};

const send = (message) => {
  if (message && ws && ws.readyState == WebSocket.OPEN) {
    ws.send(message);
  }
};

const updateState = (gameState) => {
  board = gameState.player[0].board;
  score = gameState.player[0].score;

  const prevState = state;
  state = gameState.state;
  if (state != prevState) {
    onChangedState(prevState, state);
  }
};

const onChangedState = (old, curr) => {
  if (curr == "over") {
    ws.close();
  }
  if (curr == "started") {
    startButton.textContent = "Game Active";
  }
};

const render = () => {
  clearCanvas();
  renderBoard(board);
  renderScore(score);
};

const clearCanvas = () => {
  ctx.clearRect(0, 0, canvas.width, canvas.height);
}

const renderBoard = (board) => {
  if (!board) return;
  for (let i = 0; i < board.length; i++) {
    const x = (i % 5) * cellSize;
    const y = Math.floor(i / 5) * cellSize;
    ctx.fillStyle = board[i] === 1 ? "red" : "white";
    ctx.fillRect(x, y, cellSize, cellSize);
    ctx.strokeRect(x, y, cellSize, cellSize);
  }
};

const renderScore = (score) => {
  playerScore.textContent = score;
};

const hitMole = (x, y) => {
  const i = Math.floor(x / cellSize) + Math.floor(y / cellSize) * 5;
  if (board[i] === 1) {
    send(`hit${i + 1}`);
  }
};

const enableStartButton = (enabled) => {
  if (enabled) {
    startButton.textContent = startButtonDefaultText;
  } else if (playersPerGame != undefined) {
    const playersToFind = playersPerGame - 1;
    if (playersToFind == 1) {
      startButton.textContent = `Searching for another player...`;
    } else {
      startButton.textContent = `Searching for ${playersToFind} players...`;
    }
  } else {
    startButton.textContent = "Searching for players...";
  }
  startButton.disabled = !enabled;
};

canvas.addEventListener("click", (event) => {
  const rect = canvas.getBoundingClientRect();
  const x = event.clientX - rect.left;
  const y = event.clientY - rect.top;
  hitMole(x, y);
});
startButton.addEventListener("click", startGame);

const periodicallyUpdateGameInfo = () => {
  const fetchInfo = () => {
    fetch(infoEndpoint)
      .then((response) => response.json())
      .then((data) => {
        if (data) {
          activeGameCountElement.textContent = data.active_game_count;
          websocketConnCountElement.textContent = data.websocket_conn_count;
          playersPerGame = data.players_per_game;
        }
      })
      .catch((error) => {
        console.error("Error fetching info:", error);
      });
  };
  fetchInfo();
  setInterval(fetchInfo, 5000);
};

periodicallyUpdateGameInfo();