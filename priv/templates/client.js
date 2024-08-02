const canvas = document.getElementById("gameCanvas");
const ctx = canvas.getContext("2d");
const cellSize = 100;

const startButton = document.getElementById("startButton");
const playerScore = document.getElementById("playerScore");

let board;
let score = 0;

const server = "{{ws_proto}}://" + window.location.host + "/server";
let state;
let ws;

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

  prevState = state;
  state = gameState.state;
  if (state != prevState) {
    onChangedState(prevState, state);
  }
};

const onChangedState = (old, curr) => {
  if (curr == "over") {
      ws.close();
  }
};

const render = () => {
  renderBoard(board);
  renderScore();
};

const renderBoard = (board) => {
  ctx.clearRect(0, 0, canvas.width, canvas.height);
  for (let i = 0; i < board.length; i++) {
    const x = (i % 5) * cellSize;
    const y = Math.floor(i / 5) * cellSize;
    ctx.fillStyle = board[i] === 1 ? "red" : "white";
    ctx.fillRect(x, y, cellSize, cellSize);
    ctx.strokeRect(x, y, cellSize, cellSize);
  }
};

const renderScore = () => {
  ctx.font = "20px Arial";
  ctx.fillStyle = "black";
  ctx.textAlign = "left";
  ctx.textBaseline = "top";
  ctx.fillText(`Score: ${score}`, 10, 10);
};

const hitMole = (x, y) => {
  const i = Math.floor(x / cellSize) + Math.floor(y / cellSize) * 5;
  if (board[i] === 1) {
    send(`hit${i + 1}`);
  }
};

const enableStartButton = (enabled) => {
  startButton.disabled = !enabled;
}

canvas.addEventListener("click", (event) => {
  const rect = canvas.getBoundingClientRect();
  const x = event.clientX - rect.left;
  const y = event.clientY - rect.top;
  hitMole(x, y);
});
startButton.addEventListener("click", connect);
