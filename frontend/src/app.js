import { Terminal } from "xterm";
import { FitAddon } from "xterm-addon-fit";

import "xterm/css/xterm.css";

const DEBUG = window.location.hash === "#debug";
const config = window.rijuConfig;

function closeModal() {
  document.querySelector("html").classList.remove("is-clipped");
  document.getElementById("modal").classList.remove("is-active");
}

function showError({ message, data }) {
  document.getElementById("modal-title").innerText = message;
  document.getElementById("modal-data").innerText =
    data || "(no output on stderr)";
  document.getElementById("modal").classList.add("is-active");
  document.querySelector("html").classList.add("is-clipped");
}

async function main() {
  let serviceLogBuffers = {};
  let serviceLogLines = {};

  let lastActivityTimestamp = new Date();
  let idleDueToInactivity = false;

  function recordActivity() {
    lastActivityTimestamp = new Date();
    if (idleDueToInactivity) {
      scheduleConnect();
    }
  }

  const term = new Terminal();
  const fitAddon = new FitAddon();
  term.loadAddon(fitAddon);
  term.open(document.getElementById("terminal"));

  fitAddon.fit();
  window.addEventListener("resize", () => fitAddon.fit());

  await new Promise((resolve) =>
    term.write("Connecting to server...", resolve)
  );

  const initialRetryDelayMs = 200;
  let retryDelayMs = initialRetryDelayMs;

  function sendMessage(message) {
    if (DEBUG) {
      console.log("SEND:", message);
    }
    if (socket) {
      socket.send(JSON.stringify(message));
    }
  }

  const allowedOrigins = ["http://localhost:3000", "https://codeamigo.dev"];
  window.addEventListener("message", (msg) => {
    try {
      if (allowedOrigins.indexOf(msg.origin) !== -1) {
        sendMessage(msg.data);
      }
    } catch (e) {
      console.log("message error: ", e);
    }
    console.log("message from codeamigo", msg);
  });

  function tryConnect() {
    serviceLogBuffers = {};
    serviceLogLines = {};
    let clientDisposable = null;
    let servicesDisposable = null;
    console.log("Connecting to server...");
    socket = new WebSocket(
      (document.location.protocol === "http:" ? "ws://" : "wss://") +
        document.location.host +
        `/api/v1/ws?lang=${encodeURIComponent(config.id)}`
    );
    socket.addEventListener("open", () => {
      console.log("Successfully connected to server");
    });
    socket.addEventListener("message", (event) => {
      let message;
      try {
        message = JSON.parse(event.data);
      } catch (err) {
        console.error("Malformed message from server:", event.data);
        return;
      }
      if (
        DEBUG &&
        message &&
        message.event !== "lspOutput" &&
        message.event !== "serviceLog"
      ) {
        console.log("RECEIVE:", message);
      }
      if (message && message.event && message.event !== "error") {
        retryDelayMs = initialRetryDelayMs;
      }
      switch (message && message.event) {
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
        case "testTerminalOutput":
          if (typeof message.output !== "string") {
            console.error("Unexpected message from server:", message);
            return;
          }
          term.write(message.output);

          const pass = message.output == message.expectedOutput
          
          window.parent.postMessage({
            event: "total_test_start",
            type: "test",
          }, "*");

          window.parent.postMessage({
            $id: 0,
            codesandbox: true,
            event: "test_end",
            test: {
              blocks: ["Output"],
              duration: 1,
              errors: [],
              name: `should be ${message.expectedOutput}.`,
              path: "",
              status: pass ? "pass" : "fail",
            },
            type: "test",
          }, "*");

          window.parent.postMessage({
            event: "total_test_end",
            type: "test",
          }, "*");
          return;
        case "lspStopped":
          if (clientDisposable) {
            clientDisposable.dispose();
            clientDisposable = null;
          }
          if (servicesDisposable) {
            servicesDisposable.dispose();
            servicesDisposable = null;
          }
          break;
        case "lspStarted":
          if (typeof message.root !== "string") {
            console.error("Unexpected message from server:", message);
            return;
          }
          clientDisposable = client.start();
          return;
        case "lspOutput":
          // Should be handled by RijuMessageReader
          return;
        case "serviceLog":
          if (
            typeof message.service !== "string" ||
            typeof message.output !== "string"
          ) {
            console.error("Unexpected message from server:", message);
            return;
          }
          let buffer = serviceLogBuffers[message.service] || "";
          let lines = serviceLogLines[message.service] || [];
          buffer += message.output;
          while (buffer.includes("\n")) {
            const idx = buffer.indexOf("\n");
            const line = buffer.slice(0, idx);
            buffer = buffer.slice(idx + 1);
            lines.push(line);
            if (DEBUG) {
              console.log(`${message.service.toUpperCase()} || ${line}`);
            }
          }
          serviceLogBuffers[message.service] = buffer;
          serviceLogLines[message.service] = lines;
          return;
        case "serviceFailed":
          if (
            typeof message.service !== "string" ||
            typeof message.error !== "string"
          ) {
            console.error("Unexpected message from server:", message);
            return;
          }
          switch (message.service) {
            case "formatter":
              formatButton.disabled = false;
              formatButton.classList.remove("is-loading");
              showError({
                message: "Could not prettify code!",
                data: serviceLogLines["formatter"].join("\n"),
              });
              break;
            case "lsp":
              break;
            case "terminal":
              term.write(`\r\n[${message.error}]`);
              break;
          }
          return;
        case "langConfig":
          // We could use this message instead of hardcoding the
          // language config into the HTML page returned from the
          // server, but for now we just ignore it.
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
      if (clientDisposable) {
        clientDisposable.dispose();
        clientDisposable = null;
      }
      if (servicesDisposable) {
        servicesDisposable.dispose();
        servicesDisposable = null;
      }
      scheduleConnect();
    });
  }

  function scheduleConnect() {
    idleDueToInactivity = new Date() - lastActivityTimestamp > 10 * 60 * 1000;
    if (idleDueToInactivity) {
      return;
    }
    const delay = retryDelayMs * Math.random();
    console.log(`Trying to reconnect in ${Math.floor(delay)}ms`);
    setTimeout(tryConnect, delay);
    retryDelayMs *= 2;
  }

  let socket = null;
  tryConnect();

  term.onData((data) => {
    sendMessage({ event: "terminalInput", input: data });
    recordActivity();
  });

  for (const elt of document.querySelectorAll(".will-close-modal")) {
    elt.addEventListener("click", closeModal);
  }
}

main().catch(console.error);
