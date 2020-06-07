import * as fs from "fs";
import * as pty from "node-pty";
import { IPty } from "node-pty";
import * as path from "path";
import * as tmp from "tmp";
import * as WebSocket from "ws";

import { LangConfig, langs } from "./langs";

export class Session {
  code: string;
  config: LangConfig;
  term: IPty;
  ws: WebSocket;

  constructor(ws: WebSocket, lang: string) {
    this.ws = ws;
    this.config = langs[lang];
    this.term = null;
    this.code = "";
    this.ws.send(
      JSON.stringify({
        event: "setMonacoLanguage",
        monacoLanguage: this.config.monacoLang,
      })
    );
    this.run().catch(console.error);
    ws.on("message", this.handleClientMessage);
  }
  handleClientMessage = (event: string) => {
    let msg: any;
    try {
      msg = JSON.parse(event);
    } catch (err) {
      console.error(`failed to parse client message: ${msg}`);
      return;
    }
    switch (msg?.event) {
      case "terminalInput":
        if (!this.term) {
          console.error(`terminalInput: no terminal`);
        } else if (typeof msg.input !== "string") {
          console.error(`terminalInput: missing or malformed input field`);
        } else {
          this.term.write(msg.input);
        }
        break;
      case "runCode":
        if (typeof msg.code !== "string") {
          console.error(`runCode: missing or malformed code field`);
        } else {
          this.code = msg.code;
          this.run();
        }
        break;
      default:
        console.error(`unknown client message type: ${msg.event}`);
        break;
    }
  };
  parseCmdline = (cmdline: string[] | string) => {
    if (typeof cmdline === "string") {
      return ["bash", "-c", cmdline];
    } else {
      return cmdline;
    }
  };
  run = async () => {
    const { repl, file, run } = this.config;
    if (this.term) {
      this.term.kill();
    }
    this.ws.send(JSON.stringify({ event: "terminalClear" }));
    const tmpdir: string = await new Promise((resolve, reject) =>
      tmp.dir({ unsafeCleanup: true }, (err, path) => {
        if (err) {
          reject(err);
        } else {
          resolve(path);
        }
      })
    );
    let cmdline: string[];
    if (this.code || !repl) {
      await new Promise((resolve, reject) =>
        fs.writeFile(path.resolve(tmpdir, file), this.code, (err) => {
          if (err) {
            reject(err);
          } else {
            resolve();
          }
        })
      );
      cmdline = this.parseCmdline(run);
    } else {
      cmdline = this.parseCmdline(repl);
    }
    this.term = pty.spawn(cmdline[0], cmdline.slice(1), {
      name: "xterm-color",
      cwd: tmpdir,
      env: process.env,
    });
    this.term.on("data", (data) =>
      this.ws.send(JSON.stringify({ event: "terminalOutput", output: data }))
    );
  };
}
