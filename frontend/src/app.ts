"use strict";

import * as monaco from "monaco-editor";
import { Terminal } from "xterm";
import { FitAddon } from "xterm-addon-fit";

import "xterm/css/xterm.css";

interface RijuConfig {
  id: string;
  monacoLang: string;
  template: string;
}

const config: RijuConfig = (window as any).rijuConfig;

const term = new Terminal();
const fitAddon = new FitAddon();
term.loadAddon(fitAddon);
term.open(document.getElementById("terminal"));

fitAddon.fit();
window.addEventListener("resize", () => fitAddon.fit());

term.write("Connecting to server...");

const initialRetryDelayMs = 200;
let retryDelayMs = initialRetryDelayMs;

function tryConnect() {
  console.log("Connecting to server...");
  socket = new WebSocket(
    (document.location.protocol === "http:" ? "ws://" : "wss://") +
      document.location.host +
      `/api/v1/ws?lang=${encodeURIComponent(config.id)}`
  );
  socket.addEventListener("open", () => {
    console.log("Successfully connected to server");
  });
  socket.addEventListener("message", (event: MessageEvent) => {
    let message: any;
    try {
      message = JSON.parse(event.data);
    } catch (err) {
      console.error("Malformed message from server:", event.data);
      return;
    }
    if (message?.event && message?.event !== "error") {
      retryDelayMs = initialRetryDelayMs;
    }
    switch (message?.event) {
      case "terminalClear":
        term.reset();
        return;
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
  socket.addEventListener("close", (event: CloseEvent) => {
    if (event.wasClean) {
      console.log("Connection closed cleanly");
    } else {
      console.error("Connection died");
    }
    scheduleConnect();
  });
}

function scheduleConnect() {
  const delay = retryDelayMs * Math.random();
  console.log(`Trying to reconnect in ${Math.floor(delay)}ms`);
  setTimeout(tryConnect, delay);
  retryDelayMs *= 2;
}

let socket = null;
tryConnect();

term.onData((data) =>
  socket.send(JSON.stringify({ event: "terminalInput", input: data }))
);

const editor = monaco.editor.create(document.getElementById("editor"), {
  minimap: { enabled: false },
  scrollbar: { verticalScrollbarSize: 0 },
});
window.addEventListener("resize", () => editor.layout());
editor.getModel().setValue(config.template);
monaco.editor.setModelLanguage(editor.getModel(), config.monacoLang);

document.getElementById("runButton").addEventListener("click", () => {
  socket.send(JSON.stringify({ event: "runCode", code: editor.getValue() }));
});
