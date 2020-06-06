import * as monaco from "monaco-editor";
import { Terminal } from "xterm";
import { FitAddon } from "xterm-addon-fit";

import "xterm/css/xterm.css";

const lang = document.location.pathname.slice(1);

const term = new Terminal();
const fitAddon = new FitAddon();
term.loadAddon(fitAddon);
term.open(document.getElementById("terminal"));

fitAddon.fit();
window.addEventListener("resize", () => fitAddon.fit());

const socket = new WebSocket(
  (document.location.protocol === "http:" ? "ws://" : "wss://") +
    document.location.host +
    `/api/v1/ws?lang=${lang}`
);

socket.addEventListener("open", () =>
  console.log("Successfully connected to server")
);
socket.addEventListener("message", (event) => {
  let message: any;
  try {
    message = JSON.parse(event.data);
  } catch (err) {
    console.error("Malformed message from server:", event.data);
    return;
  }
  switch (message?.event) {
    case "terminalOutput":
      if (typeof message.output !== "string") {
        console.error("Unexpected message from server:", message);
        return;
      }
      term.write(message.output);
      return;
    default:
      console.error("Unexpected message from server:", message);
      return;
  }
});
socket.addEventListener("close", (event) => {
  if (event.wasClean) {
    console.log("Connection closed cleanly");
  } else {
    console.error("Connection died");
  }
});
socket.addEventListener("onerror", (event) =>
  console.error("Connection error:", event)
);

term.onData((data) =>
  socket.send(JSON.stringify({ event: "terminalInput", input: data }))
);

monaco.editor.create(document.getElementById("editor"));
