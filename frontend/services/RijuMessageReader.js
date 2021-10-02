import { AbstractMessageReader } from "vscode-jsonrpc/lib/messageReader";

const DEBUG = window.location.hash === "#debug";

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

export default RijuMessageReader;
