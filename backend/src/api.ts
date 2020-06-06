import * as pty from "node-pty";
import { IPty } from "node-pty";
import * as WebSocket from "ws";

import { LangConfig, langs } from "./langs";

export class Session {
  config: LangConfig;
  term: IPty;
  ws: WebSocket;

  constructor(ws: WebSocket, lang: string) {
    this.ws = ws;
    this.config = langs[lang];
    this.term = null;
    this.run();
    ws.on("message", this.handleClientMessage);
  }
  handleClientMessage(msg) {
    try {
      msg = JSON.parse(msg);
    } catch (err) {
      console.error(`failed to parse client message: ${msg}`);
      return;
    }
    switch (msg.event) {
      case "terminalInput":
        if (!this.term) {
          console.error(`terminalInput: no terminal`);
        } else if (typeof msg.input !== "string") {
          console.error(`terminalInput: missing or malformed input field`);
        } else {
          this.term.write(msg.input);
        }
        break;
      default:
        console.error(`unknown client message type: ${msg.event}`);
        break;
    }
  }
  run() {
    const cmdline = this.config.cmdline;
    if (this.term) {
      this.term.kill();
    }
    this.term = pty.spawn(cmdline[0], cmdline.slice(1), {
      name: "xterm-color",
      cwd: process.env.PWD,
      env: process.env,
    });
    this.term.on("data", (data) =>
      this.ws.send(JSON.stringify({ event: "terminalOutput", output: data }))
    );
  }
}
