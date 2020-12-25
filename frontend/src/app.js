import monaco from "monaco-editor";
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

class RijuMessageReader extends AbstractMessageReader {
  constructor(socketSocket) {
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
    let clientDisposable = null;
    let servicesDisposable = null;
    const serviceLogBuffers = {};
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
        case "formattedCode":
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
        case "lspStarted":
          if (typeof message.root !== "string") {
            console.error("Unexpected message from server:", message);
            return;
          }
          const services = MonacoServices.create(editor, {
            rootUri: `file://${message.root}`,
          });
          servicesDisposable = Services.install(services);
          editor.setModel(
            monaco.editor.createModel(
              editor.getModel().getValue(),
              undefined,
              monaco.Uri.parse(`file://${message.root}/${config.main}`)
            )
          );
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
          if (DEBUG) {
            let buffer = serviceLogBuffers[message.service] || "";
            buffer += message.output;
            while (buffer.includes("\n")) {
              const idx = buffer.indexOf("\n");
              const line = buffer.slice(0, idx);
              buffer = buffer.slice(idx + 1);
              console.log(`${message.service.toUpperCase()} || ${line}`);
            }
            serviceLogBuffers[message.service] = buffer;
          }
          return;
        case "serviceCrashed":
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
  window.addEventListener("resize", () => editor.layout());
  editor.getModel().setValue(config.template);
  monaco.editor.setModelLanguage(
    editor.getModel(),
    config.monacoLang || "plaintext"
  );

  document.getElementById("runButton").addEventListener("click", () => {
    sendMessage({ event: "runCode", code: editor.getValue() });
  });
  if (config.format) {
    document.getElementById("formatButton").classList.add("visible");
    document.getElementById("formatButton").addEventListener("click", () => {
      sendMessage({ event: "formatCode", code: editor.getValue() });
    });
  }
}

main().catch(console.error);
