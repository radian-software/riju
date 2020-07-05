import { ChildProcess, spawn } from "child_process";
import * as path from "path";
import * as WebSocket from "ws";

import * as pty from "node-pty";
import { IPty } from "node-pty";
import * as rpc from "vscode-jsonrpc";
import { v4 as getUUID } from "uuid";

import { PRIVILEGED } from "./config";
import { LangConfig, langs } from "./langs";
import { borrowUser } from "./users";
import {
  callPrivileged,
  getEnv,
  rijuSystemPrivileged,
  spawnPrivileged,
} from "./util";

export class Session {
  uuid: string;
  code: string;
  config: LangConfig;
  term: { pty: IPty | null; live: boolean };
  lsp: {
    proc: ChildProcess;
    reader: rpc.StreamMessageReader;
    writer: rpc.StreamMessageWriter;
  } | null;
  ws: WebSocket;
  homedir: string | null;
  uid: number | null;
  uidCleanup: (() => Promise<void>) | null;

  log = (msg: string) => console.log(`[${this.uuid}] ${msg}`);

  constructor(ws: WebSocket, lang: string) {
    this.uuid = getUUID();
    this.log(`Creating session, language ${lang}`);
    this.ws = ws;
    this.config = langs[lang];
    this.term = { pty: null, live: false };
    this.lsp = null;
    this.code = "";
    this.homedir = null;
    this.uid = null;
    this.uidCleanup = null;
    ws.on("message", this.handleClientMessage);
    ws.on("close", () =>
      this.cleanup().catch((err) => {
        this.log(`Error during session cleanup`);
        console.log(err);
      })
    );
    this.run().catch((err) => {
      this.log(`Error while setting up environment for pty`);
      console.log(err);
      try {
        this.ws.send(JSON.stringify({ event: "terminalClear" }));
        this.ws.send(
          JSON.stringify({
            event: "terminalOutput",
            output: `Riju encountered an unexpected error: ${err}
\rYou may want to save your code and refresh the page.
`,
          })
        );
      } catch (err) {
        //
      }
    });
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
      case "lspInput":
        if (!this.lsp) {
          this.log(`Got LSP input before language server was started`);
        } else {
          this.lsp.writer.write(msg.input);
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
      this.log(`Borrowed uid ${this.uid}`);
    }
    const {
      name,
      repl,
      main,
      suffix,
      alwaysCreate,
      compile,
      run,
      lsp,
      hacks,
    } = this.config;
    if (this.term.pty) {
      this.term.pty.kill();
      this.term.live = false;
    }
    try {
      this.ws.send(JSON.stringify({ event: "terminalClear" }));
    } catch (err) {
      //
    }
    if (this.homedir == null) {
      this.homedir = `/tmp/riju/${this.uuid}`;
      await callPrivileged(["setup", `${this.uid}`, this.uuid], this.log);
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
        await spawnPrivileged(
          this.uid,
          this.uuid,
          ["mkdir", "-p", path.dirname(path.resolve(this.homedir, main))],
          this.log
        );
      }
      await spawnPrivileged(
        this.uid,
        this.uuid,
        ["sh", "-c", `cat > ${path.resolve(this.homedir, main)}`],
        this.log,
        { input: code }
      );
      cmdline = run;
      if (compile) {
        cmdline = `( ${compile} ) && ( ${run} )`;
      }
    } else if (repl) {
      if (alwaysCreate) {
        await spawnPrivileged(
          this.uid,
          this.uuid,
          ["touch", `${path.resolve(this.homedir, main)}`],
          this.log
        );
      }
      cmdline = repl;
    } else {
      cmdline = `echo '${name} has no REPL, press Run to see it in action'`;
    }
    if (hacks && hacks.includes("ghci-config") && run) {
      if (this.code) {
        const contents = ":load Main\nmain\n";
        await spawnPrivileged(
          this.uid,
          this.uuid,
          ["sh", "-c", `cat > ${path.resolve(this.homedir, ".ghci")}`],
          this.log,
          { input: contents }
        );
      } else {
        await spawnPrivileged(
          this.uid,
          this.uuid,
          ["rm", "-f", path.resolve(this.homedir, ".ghci")],
          this.log
        );
      }
    }
    const args = [
      rijuSystemPrivileged,
      "spawn",
      `${this.uid}`,
      `${this.uuid}`,
      "bash",
      "-c",
      cmdline,
    ];
    const env = getEnv(this.uuid);
    const term = {
      pty: pty.spawn(args[0], args.slice(1), {
        name: "xterm-color",
        env,
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
    if (lsp && this.lsp === null) {
      const lspArgs = [
        rijuSystemPrivileged,
        "spawn",
        `${this.uid}`,
        `${this.uuid}`,
        "bash",
        "-c",
        lsp,
      ];
      const proc = spawn(lspArgs[0], lspArgs.slice(1), {
        env: getEnv(this.uuid),
      });
      this.lsp = {
        proc,
        reader: new rpc.StreamMessageReader(proc.stdout),
        writer: new rpc.StreamMessageWriter(proc.stdin),
      };
      this.lsp.reader.listen((data) => {
        this.ws.send(JSON.stringify({ event: "lspOutput", output: data }));
      });
      this.ws.send(JSON.stringify({ event: "lspStarted" }));
    }
  };
  cleanup = async () => {
    this.log(`Cleaning up session`);
    if (this.term.pty) {
      await spawnPrivileged(
        this.uid!,
        this.uuid,
        ["kill", "-9", `${this.term.pty.pid}`],
        this.log
      );
    }
    if (this.lsp !== null) {
      await spawnPrivileged(
        this.uid!,
        this.uuid,
        ["kill", "-9", `${this.lsp.proc.pid}`],
        this.log
      );
    }
    if (this.homedir) {
      await callPrivileged(["teardown", this.uuid], this.log);
    }
    if (this.uidCleanup) {
      await this.uidCleanup();
      this.log(`Returned uid ${this.uid}`);
    }
  };
}
