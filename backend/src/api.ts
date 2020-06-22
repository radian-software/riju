import * as fs from "fs";
import * as path from "path";
import * as WebSocket from "ws";

import * as mkdirp from "mkdirp";
import * as nodeCleanup from "node-cleanup";
import * as pty from "node-pty";
import { IPty } from "node-pty";
import * as tmp from "tmp";
import { v4 as getUUID } from "uuid";

import { LangConfig, langs } from "./langs";
import { borrowUser } from "./users";

export class Session {
  id: string;
  code: string;
  config: LangConfig;
  term: { pty: IPty | null; live: boolean };
  ws: WebSocket;
  tmpdir: string | null;
  tmpdirCleanup: (() => void) | null;
  uid: number | null;
  uidCleanup: (() => Promise<void>) | null;

  log = (msg: string) => console.log(`[${this.id}] ${msg}`);

  constructor(ws: WebSocket, lang: string) {
    this.id = getUUID();
    this.log(`Creating session, language ${lang}`);
    this.ws = ws;
    this.config = langs[lang];
    this.term = { pty: null, live: false };
    this.code = "";
    this.tmpdir = null;
    this.tmpdirCleanup = null;
    this.uid = null;
    this.uidCleanup = null;
    ws.on("message", this.handleClientMessage);
    ws.on("close", () =>
      this.cleanup().catch((err) =>
        this.log(`Error during session cleanup: ${err}`)
      )
    );
    nodeCleanup();
    this.run().catch((err) => this.log(`Error while running: ${err}`));
  }
  handleClientMessage = (event: string) => {
    let msg: any;
    try {
      msg = JSON.parse(event);
    } catch (err) {
      this.log(`Failed to parse client message: ${msg}`);
      return;
    }
    switch (msg?.event) {
      case "terminalInput":
        if (!this.term) {
          this.log(`Got terminal input before pty was started`);
        } else if (typeof msg.input !== "string") {
          this.log(`Got malformed terminal input message`);
        } else {
          this.term.pty!.write(msg.input);
        }
        break;
      case "runCode":
        if (typeof msg.code !== "string") {
          this.log(`Got malformed run message`);
        } else {
          this.code = msg.code;
          this.run();
        }
        break;
      default:
        this.log(`Got unknown message type: ${msg.event}`);
        break;
    }
  };
  run = async () => {
    if (this.uid === null) {
      ({ uid: this.uid, cleanup: this.uidCleanup } = await borrowUser(
        this.log
      ));
    }
    this.log(`Borrowed uid ${this.uid}`);
    const { name, repl, main, suffix, compile, run, hacks } = this.config;
    if (this.term.pty) {
      this.term.pty.kill();
      this.term.live = false;
    }
    try {
      this.ws.send(JSON.stringify({ event: "terminalClear" }));
    } catch (err) {
      //
    }
    if (this.tmpdir == null) {
      ({ path: this.tmpdir, cleanup: this.tmpdirCleanup } = await new Promise(
        (resolve, reject) =>
          tmp.dir(
            { unsafeCleanup: true, dir: "riju" },
            (err, path, cleanup) => {
              if (err) {
                reject(err);
              } else {
                resolve({ path, cleanup });
              }
            }
          )
      ));
    }
    let cmdline: string;
    if (!run) {
      cmdline = `echo 'Support for ${this.config.name} is not yet implemented.'`;
    } else if (this.code) {
      let code = this.code;
      if (suffix) {
        code += suffix;
      }
      if (main.includes("/")) {
        await mkdirp(path.dirname(path.resolve(this.tmpdir!, main)));
      }
      await new Promise((resolve, reject) =>
        fs.writeFile(path.resolve(this.tmpdir!, main), code, (err) => {
          if (err) {
            reject(err);
          } else {
            resolve();
          }
        })
      );
      cmdline = run;
      if (compile) {
        cmdline = `( ${compile} ) && ( ${run} )`;
      }
    } else if (repl) {
      cmdline = repl;
    } else {
      cmdline = `echo '${name} has no REPL, press Run to see it in action'`;
    }
    if (hacks && hacks.includes("ghci-config") && run) {
      if (this.code) {
        const contents = ":load Main\nmain\n";
        await new Promise((resolve, reject) => {
          fs.writeFile(path.resolve(this.tmpdir!, ".ghci"), contents, (err) => {
            if (err) {
              reject(err);
            } else {
              resolve();
            }
          });
        });
      } else {
        await new Promise((resolve, reject) =>
          fs.unlink(path.resolve(this.tmpdir!, ".ghci"), (err) => {
            if (err && err.code !== "ENOENT") {
              reject(err);
            } else {
              resolve();
            }
          })
        );
      }
    }
    const term = {
      pty: pty.spawn("bash", ["-c", cmdline], {
        name: "xterm-color",
        cwd: this.tmpdir!,
        env: process.env as { [key: string]: string },
      }),
      live: true,
    };
    this.term = term;
    term.pty.on("data", (data) => {
      // Capture term in closure so that we don't keep sending output
      // from the old pty even after it's been killed (see ghci).
      if (term.live) {
        try {
          this.ws.send(
            JSON.stringify({ event: "terminalOutput", output: data })
          );
        } catch (err) {
          //
        }
      }
    });
  };
  cleanup = async () => {
    this.log(`Cleaning up session`);
    if (this.tmpdirCleanup) {
      this.tmpdirCleanup();
    }
    if (this.uidCleanup) {
      await this.uidCleanup();
    }
  };
}
