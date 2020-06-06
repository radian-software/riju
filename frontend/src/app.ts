import { Terminal } from "xterm";
import { FitAddon } from "xterm-addon-fit";

import "xterm/css/xterm.css";

const term = new Terminal();
const fitAddon = new FitAddon();
term.loadAddon(fitAddon);
term.open(document.getElementById("terminal"));

fitAddon.fit();
window.addEventListener("resize", () => fitAddon.fit());

const socket = new WebSocket(
  (document.location.protocol === "http:" ? "ws://" : "wss://") +
    document.location.host +
    "/api/v1/ws"
);

socket.onopen = () => console.log("Successfully connected to server");
socket.onmessage = (event) => console.log(event);
socket.onclose = (event) => {
  if (event.wasClean) {
    console.log("Connection closed cleanly");
  } else {
    console.error("Connection died");
  }
};
socket.onerror = (event) => console.error("Connection error:", event);
