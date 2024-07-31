const server = "ws://" + window.location.host + "/server";
const ws = new WebSocket(server);

ws.onopen = function (evt) {
  console.log("[ws][open]: " + evt.data);
  ws.send("ready"); // TODO do this on button
};
ws.onclose = function (evt) {
  console.log("[ws][close]: " + evt.data);
};
ws.onmessage = function (evt) {
  console.log("[ws][message]: " + evt.data);
};
ws.onerror = function (evt) {
  console.log("[ws][error]: " + evt.data);
};
