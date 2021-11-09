import { AbstractMessageWriter } from "vscode-jsonrpc/lib/messageWriter";

const DEBUG = window.location.hash === "#debug";

class RijuMessageWriter extends AbstractMessageWriter {
  constructor(socket, config) {
    super();
    this.socket = socket;
    this.config = config;
  }

  write(msg) {
    switch (msg.method) {
      case "initialize":
        msg.params.processId = null;
        if (this.config.lsp.disableDynamicRegistration) {
          this.disableDynamicRegistration(msg);
        }
        break;
      case "textDocument/didOpen":
        if (this.config.lsp.lang) {
          msg.params.textDocument.languageId = this.config.lsp.lang;
        }
    }
    if (DEBUG) {
      console.log("SEND LSP:", msg);
    }
    if (this.socket && this.socket.readyState === WebSocket.OPEN)
      this.socket.send(JSON.stringify({ event: "lspInput", input: msg }));
  }

  disableDynamicRegistration(msg) {
    if (!msg || typeof msg !== "object") return;
    for (const [key, val] of Object.entries(msg)) {
      if (key === "dynamicRegistration" && val === true) {
        msg.dynamicRegistration = false;
      }
      this.disableDynamicRegistration(val);
    }
  }
}

export default RijuMessageWriter;
