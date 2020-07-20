import { ChildProcess, spawn } from "child_process";
import * as path from "path";
import * as WebSocket from "ws";

import * as pty from "node-pty";
import { IPty } from "node-pty";
import * as rpc from "vscode-jsonrpc";
import { v4 as getUUID } from "uuid";

import { LangConfig, langs } from "./langs";
import { borrowUser } from "./users";
import * as util from "./util";
import { Context, Options, bash } from "./util";

const allSessions: Set<Session> = new Set();

export class Session {
  ws: WebSocket;
  uuid: string;
  lang: string;

  tearingDown: boolean = false;

  // Initialized by setup()
  uidInfo: {
    uid: number;
    returnUID: () => Promise<void>;
  } | null = null;

  // Initialized later or never
  term: { pty: IPty; live: boolean } | null = null;
  lsp: {
    proc: ChildProcess;
    reader: rpc.StreamMessageReader;
    writer: rpc.StreamMessageWriter;
  } | null = null;
  daemon: { proc: ChildProcess } | null = null;
  formatter: {
    proc: ChildProcess;
    live: boolean;
    input: string;
    output: string;
  } | null = null;

  get homedir() {
    return `/tmp/riju/${this.uuid}`;
  }

  get config() {
    return langs[this.lang];
  }

  get uid() {
    return this.uidInfo!.uid;
  }

  returnUID = async () => {
    this.uidInfo && (await this.uidInfo.returnUID());
  };

  get context() {
    return { uid: this.uid, uuid: this.uuid };
  }

  log = (msg: string) => console.log(`[${this.uuid}] ${msg}`);

  constructor(ws: WebSocket, lang: string) {
    this.ws = ws;
    this.uuid = getUUID();
    this.lang = lang;
    this.log(`Creating session, language ${this.lang}`);
    this.setup();
  }

  run = async (args: string[], options?: Options) => {
    return await util.run(args, this.log, options);
  };

  privilegedSetup = () => util.privilegedSetup(this.context);
  privilegedSpawn = (args: string[]) =>
    util.privilegedSpawn(this.context, args);
  privilegedUseradd = () => util.privilegedUseradd(this.uid);
  privilegedTeardown = () => util.privilegedTeardown(this.context);

  setup = async () => {
    try {
      allSessions.add(this);
      const { uid, returnUID } = await borrowUser(this.log);
      this.uidInfo = { uid, returnUID };
      this.log(`Borrowed uid ${this.uid}`);
      await this.run(this.privilegedSetup());
      if (this.config.setup) {
        await this.run(this.privilegedSpawn(bash(this.config.setup)));
      }
      await this.runCode();
      if (this.config.daemon) {
        const daemonArgs = this.privilegedSpawn(bash(this.config.daemon));
        const daemonProc = spawn(daemonArgs[0], daemonArgs.slice(1));
        this.daemon = {
          proc: daemonProc,
        };
        for (const stream of [daemonProc.stdout, daemonProc.stderr]) {
          stream.on("data", (data) =>
            this.send({
              event: "serviceLog",
              service: "daemon",
              output: data.toString("utf8"),
            })
          );
          daemonProc.on("exit", (code, signal) =>
            this.send({
              event: "serviceFailed",
              service: "daemon",
              error: `Exited with status ${signal || code}`,
            })
          );
          daemonProc.on("error", (err) =>
            this.send({
              event: "serviceFailed",
              service: "daemon",
              error: `${err}`,
            })
          );
        }
      }
      if (this.config.lsp) {
        if (this.config.lspSetup) {
          await this.run(this.privilegedSpawn(bash(this.config.lspSetup)));
        }
        const lspArgs = this.privilegedSpawn(bash(this.config.lsp));
        const lspProc = spawn(lspArgs[0], lspArgs.slice(1));
        this.lsp = {
          proc: lspProc,
          reader: new rpc.StreamMessageReader(lspProc.stdout),
          writer: new rpc.StreamMessageWriter(lspProc.stdin),
        };
        this.lsp.reader.listen((data: any) => {
          this.send({ event: "lspOutput", output: data });
        });
        lspProc.stderr.on("data", (data) =>
          this.send({
            event: "serviceLog",
            service: "lsp",
            output: data.toString("utf8"),
          })
        );
        lspProc.on("exit", (code, signal) =>
          this.send({
            event: "serviceFailed",
            service: "lsp",
            error: `Exited with status ${signal || code}`,
          })
        );
        lspProc.on("error", (err) =>
          this.send({ event: "serviceFailed", service: "lsp", error: `${err}` })
        );
        this.send({ event: "lspStarted", root: this.homedir });
      }
      this.ws.on("message", this.receive);
      this.ws.on("close", async () => {
        await this.teardown();
      });
      this.ws.on("error", async (err) => {
        this.log(`Websocket error: ${err}`);
        await this.teardown();
      });
    } catch (err) {
      this.log(`Error while setting up environment`);
      console.log(err);
      this.sendError(err);
      await this.teardown();
    }
  };

  send = async (msg: any) => {
    try {
      if (this.tearingDown) {
        return;
      }
      this.ws.send(JSON.stringify(msg));
    } catch (err) {
      this.log(`Failed to send websocket message: ${err}`);
      await this.teardown();
    }
  };

  sendError = async (err: any) => {
    await this.send({ event: "terminalClear" });
    await this.send({
      event: "terminalOutput",
      output: `Riju encountered an unexpected error: ${err}
\r
\rYou may want to save your code and refresh the page.
`,
    });
  };

  logBadMessage = (msg: any) => {
    this.log(`Got malformed message from client: ${JSON.stringify(msg)}`);
  };

  receive = async (event: string) => {
    try {
      if (this.tearingDown) {
        return;
      }
      let msg: any;
      try {
        msg = JSON.parse(event);
      } catch (err) {
        this.log(`Failed to parse message from client: ${event}`);
        return;
      }
      switch (msg && msg.event) {
        case "terminalInput":
          if (typeof msg.input !== "string") {
            this.logBadMessage(msg);
            break;
          }
          if (!this.term) {
            this.log("terminalInput ignored because term is null");
            break;
          }
          this.term!.pty.write(msg.input);
          break;
        case "runCode":
          if (typeof msg.code !== "string") {
            this.logBadMessage(msg);
            break;
          }
          await this.runCode(msg.code);
          break;
        case "formatCode":
          if (typeof msg.code !== "string") {
            this.logBadMessage(msg);
            break;
          }
          await this.formatCode(msg.code);
          break;
        case "lspInput":
          if (typeof msg.input !== "object" || !msg) {
            this.logBadMessage(msg);
            break;
          }
          if (!this.lsp) {
            this.log(`lspInput ignored because lsp is null`);
            break;
          }
          this.lsp.writer.write(msg.input);
          break;
        default:
          this.logBadMessage(msg);
          break;
      }
    } catch (err) {
      this.log(`Error while handling message from client`);
      console.log(err);
      this.sendError(err);
    }
  };

  writeCode = async (code: string) => {
    if (this.config.main.includes("/")) {
      await this.run(
        this.privilegedSpawn([
          "mkdir",
          "-p",
          path.dirname(`${this.homedir}/${this.config.main}`),
        ])
      );
    }
    await this.run(
      this.privilegedSpawn([
        "sh",
        "-c",
        `cat > ${path.resolve(this.homedir, this.config.main)}`,
      ]),
      { input: code }
    );
  };

  runCode = async (code?: string) => {
    try {
      const {
        name,
        repl,
        main,
        suffix,
        createEmpty,
        compile,
        run,
        template,
      } = this.config;
      if (this.term) {
        const pid = this.term.pty.pid;
        const args = this.privilegedSpawn(
          bash(`kill -SIGTERM ${pid}; sleep 1; kill -SIGKILL ${pid}`)
        );
        spawn(args[0], args.slice(1));
        // Signal to terminalOutput message generator using closure.
        this.term.live = false;
        this.term = null;
      }
      this.send({ event: "terminalClear" });
      let cmdline: string;
      if (code) {
        cmdline = run;
        if (compile) {
          cmdline = `( ${compile} ) && ( ${run} )`;
        }
      } else if (repl) {
        cmdline = repl;
      } else {
        cmdline = `echo '${name} has no REPL, press Run to see it in action'`;
      }
      if (code === undefined) {
        code = createEmpty !== undefined ? createEmpty : template;
      }
      if (code && suffix) {
        code += suffix;
      }
      await this.writeCode(code);
      const termArgs = this.privilegedSpawn(bash(cmdline));
      const term = {
        pty: pty.spawn(termArgs[0], termArgs.slice(1), {
          name: "xterm-color",
        }),
        live: true,
      };
      this.term = term;
      this.term.pty.on("data", (data) => {
        // Capture term in closure so that we don't keep sending output
        // from the old pty even after it's been killed (see ghci).
        if (term.live) {
          this.send({ event: "terminalOutput", output: data });
        }
      });
    } catch (err) {
      this.log(`Error while running user code`);
      console.log(err);
      this.sendError(err);
    }
  };

  formatCode = async (code: string) => {
    try {
      if (!this.config.format) {
        this.log("formatCode ignored because format is null");
        return;
      }
      if (this.formatter) {
        const pid = this.formatter.proc.pid;
        const args = this.privilegedSpawn(
          bash(`kill -SIGTERM ${pid}; sleep 1; kill -SIGKILL ${pid}`)
        );
        spawn(args[0], args.slice(1));
        this.formatter.live = false;
        this.formatter = null;
      }
      await this.writeCode(code);
      const args = this.privilegedSpawn(bash(this.config.format));
      const formatter = {
        proc: spawn(args[0], args.slice(1)),
        live: true,
        input: code,
        output: "",
      };
      formatter.proc.stdout!.on("data", (data) => {
        if (!formatter.live) return;
        formatter.output += data.toString("utf8");
      });
      formatter.proc.stderr!.on("data", (data) => {
        if (!formatter.live) return;
        this.send({
          event: "serviceLog",
          service: "formatter",
          output: data.toString("utf8"),
        });
      });
      formatter.proc.on("exit", (code, signal) => {
        if (!formatter.live) return;
        if (code === 0) {
          this.send({
            event: "formattedCode",
            code: formatter.output,
            originalCode: formatter.input,
          });
        } else {
          this.send({
            event: "serviceFailed",
            service: "formatter",
            error: `Exited with status ${signal || code}`,
          });
        }
      });
      formatter.proc.on("error", (err) => {
        if (!formatter.live) return;
        this.send({
          event: "serviceFailed",
          service: "formatter",
          error: `${err}`,
        });
      });
      this.formatter = formatter;
    } catch (err) {
      this.log(`Error while running code formatter`);
      console.log(err);
      this.sendError(err);
    }
  };

  teardown = async () => {
    try {
      if (this.tearingDown) {
        return;
      }
      this.log(`Tearing down session`);
      this.tearingDown = true;
      allSessions.delete(this);
      await new Promise((resolve) => setTimeout(resolve, 5000));
      await this.run(this.privilegedTeardown());
      await this.returnUID();
      this.ws.terminate();
    } catch (err) {
      this.log(`Error during teardown`);
      console.log(err);
    }
  };
}
