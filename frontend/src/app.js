import * as monaco from "monaco-editor";
import {
  createConnection,
  MonacoLanguageClient,
  MonacoServices,
  Services,
} from "monaco-languageclient";
import { Disposable } from "vscode";
import { createMessageConnection } from "vscode-jsonrpc";
import {
  AbstractMessageReader,
  DataCallback,
} from "vscode-jsonrpc/lib/messageReader.js";
import { AbstractMessageWriter } from "vscode-jsonrpc/lib/messageWriter.js";
import { Message } from "vscode-jsonrpc/lib/messages.js";
import { Terminal } from "xterm";
import { FitAddon } from "xterm-addon-fit";

import "xterm/css/xterm.css";

const DEBUG = window.location.hash === "#debug";
const config = window.rijuConfig;

const formatButton = document.getElementById("formatButton");
const lspButton = document.getElementById("lspButton");
const lspButtonState = document.getElementById("lspButtonState");
const connectionStatus = document.getElementById("connectionStatus");

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

class RijuMessageReader extends AbstractMessageReader {
  constructor(socket) {
    super();
    this.state = "initial";
    this.callback = null;
    this.messageQueue = [];
    this.socket = socket;
    this.socket.addEventListener("message", (event) => {
      this.readMessage(event.data);
    });
  }

  listen(callback) {
    if (this.state === "initial") {
      this.state = "listening";
      this.callback = callback;
      while (this.messageQueue.length > 0) {
        this.readMessage(this.messageQueue.pop());
      }
    }
  }

  readMessage(rawMessage) {
    if (this.state === "initial") {
      this.messageQueue.splice(0, 0, rawMessage);
    } else if (this.state === "listening") {
      let message;
      try {
        message = JSON.parse(rawMessage);
      } catch (err) {
        return;
      }
      switch (message && message.event) {
        case "lspOutput":
          if (DEBUG) {
            console.log("RECEIVE LSP:", message.output);
          }
          this.callback(message.output);
          break;
      }
    }
  }
}

class RijuMessageWriter extends AbstractMessageWriter {
  constructor(socket) {
    super();
    this.socket = socket;
  }

  write(msg) {
    switch (msg.method) {
      case "initialize":
        msg.params.processId = null;
        if (config.lsp.disableDynamicRegistration) {
          this.disableDynamicRegistration(msg);
        }
        break;
      case "textDocument/didOpen":
        if (config.lsp.lang) {
          msg.params.textDocument.languageId = config.lsp.lang;
        }
    }
    if (DEBUG) {
      console.log("SEND LSP:", msg);
    }
    this.socket.send(JSON.stringify({ event: "lspInput", input: msg }));
  }

  disableDynamicRegistration(msg) {
    if (!msg || typeof msg !== "object") return;
    for (const [key, val] of Object.entries(msg)) {
      if (key === "dynamicRegistration" && val === true)
        msg.dynamicRegistration = false;
      this.disableDynamicRegistration(val);
    }
  }
}

async function main() {
  let serviceLogBuffers = {};
  let serviceLogLines = {};

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

  function tryConnect() {
    serviceLogBuffers = {};
    serviceLogLines = {};
    let clientDisposable = null;
    let servicesDisposable = null;
    connectionStatus.innerText = "connecting...";
    console.log("Connecting to server...");
    socket = new WebSocket(
      (document.location.protocol === "http:" ? "ws://" : "wss://") +
        document.location.host +
        `/api/v1/ws?lang=${encodeURIComponent(config.id)}`
    );
    socket.addEventListener("open", () => {
      connectionStatus.innerText = "connected";
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
        case "formattedCode":
          formatButton.disabled = false;
          formatButton.classList.remove("is-loading");
          if (
            typeof message.code !== "string" ||
            typeof message.originalCode !== "string"
          ) {
            console.error("Unexpected message from server:", message);
            return;
          }
          if (editor.getValue() === message.originalCode) {
            editor.setValue(message.code);
          }
          return;
        case "lspStopped":
          lspButton.disabled = false;
          lspButton.classList.remove("is-loading");
          lspButton.classList.add("is-light");
          lspButtonState.innerText = "OFF";
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
          lspButton.disabled = false;
          lspButton.classList.remove("is-loading");
          lspButton.classList.remove("is-light");
          lspButtonState.innerText = "ON";
          if (typeof message.root !== "string") {
            console.error("Unexpected message from server:", message);
            return;
          }
          const services = MonacoServices.create(editor, {
            rootUri: `file://${message.root}`,
          });
          servicesDisposable = Services.install(services);
          const newURI = `file://${message.root}/${config.main}`;
          const oldModel = editor.getModel();
          if (oldModel.uri.toString() !== newURI) {
            // This code is likely to be buggy as it will probably
            // never run and has thus never been tested.
            editor.setModel(
              monaco.editor.createModel(
                oldModel.getValue(),
                undefined,
                monaco.Uri.parse(newURI)
              )
            );
            oldModel.dispose();
          }
          const connection = createMessageConnection(
            new RijuMessageReader(socket),
            new RijuMessageWriter(socket)
          );
          const client = new MonacoLanguageClient({
            name: "Riju",
            clientOptions: {
              documentSelector: [{ pattern: "**" }],
              middleware: {
                workspace: {
                  configuration: (params, token, configuration) => {
                    return Array(configuration(params, token).length).fill(
                      config.lsp.config !== undefined ? config.lsp.config : {}
                    );
                  },
                },
              },
              initializationOptions: config.lsp.init || {},
            },
            connectionProvider: {
              get: (errorHandler, closeHandler) =>
                Promise.resolve(
                  createConnection(connection, errorHandler, closeHandler)
                ),
            },
          });
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
              lspButton.disabled = false;
              lspButton.classList.remove("is-loading");
              lspButton.classList.add("is-light");
              lspButtonState.innerText = "CRASHED";
              break;
            case "terminal":
              term.write(`\r\n[${message.error}]`);
              break;
          }
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
      if (lspButtonState.innerText === "ON") {
        lspButton.disabled = false;
        lspButton.classList.remove("is-loading");
        lspButton.classList.add("is-light");
        lspButtonState.innerText = "DISCONNECTED";
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

  term.onData((data) => sendMessage({ event: "terminalInput", input: data }));

  const editor = monaco.editor.create(document.getElementById("editor"), {
    minimap: { enabled: false },
    scrollbar: { verticalScrollbarSize: 0 },
  });
  editor.addAction({
    id: "runCode",
    label: "Run",
    keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter],
    contextMenuGroupId: "2_execution",
    run: () => {
      sendMessage({ event: "runCode", code: editor.getValue() });
    },
  });
  window.addEventListener("resize", () => editor.layout());
  editor.getModel().setValue(config.template + "\n");
  monaco.editor.setModelLanguage(
    editor.getModel(),
    config.monacoLang || "plaintext"
  );

  document.getElementById("runButton").addEventListener("click", () => {
    sendMessage({ event: "runCode", code: editor.getValue() });
  });
  if (config.format) {
    formatButton.classList.remove("is-hidden");
    formatButton.addEventListener("click", () => {
      formatButton.classList.add("is-loading");
      formatButton.disabled = true;
      serviceLogBuffers["formatter"] = "";
      serviceLogLines["formatter"] = [];
      sendMessage({ event: "formatCode", code: editor.getValue() });
    });
  }
  if (config.lsp) {
    lspButton.classList.remove("is-hidden");
    lspButton.addEventListener("click", () => {
      lspButton.classList.add("is-loading");
      lspButton.disabled = true;
      lspButton.classList.remove("is-light");
      if (lspButtonState.innerText === "ON") {
        sendMessage({ event: "lspStop" });
      } else {
        serviceLogBuffers["lsp"] = "";
        serviceLogLines["lsp"] = [];
        sendMessage({ event: "lspStart" });
      }
    });
  }

  for (const elt of document.querySelectorAll(".will-close-modal")) {
    elt.addEventListener("click", closeModal);
  }
}

main().catch(console.error);
