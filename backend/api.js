import { spawn } from "child_process";
import path from "path";
import process from "process";

import pQueue from "p-queue";
const PQueue = pQueue.default;
import rpc from "vscode-jsonrpc";
import WebSocket from "ws";

import { langs } from "./langs.js";
import * as util from "./util.js";
import { bash, getUUID, logError } from "./util.js";

const allSessions = new Set();
const TEST_RUN_FINISHED = "Test run finished!";
export class Session {
  get homedir() {
    return "/home/riju/src";
  }

  get config() {
    return langs[this.lang];
  }

  get context() {
    return { uuid: this.uuid, lang: this.lang };
  }

  log = (msg) => this.logPrimitive(`[${this.uuid}] ${msg}`);

  constructor(ws, lang, log) {
    this.ws = ws;
    this.uuid = getUUID();
    this.lang = lang;
    this.tearingDown = false;
    this.container = null;
    this.term = null;
    this.lsp = null;
    this.daemon = null;
    this.formatter = null;
    this.logPrimitive = log;
    this.msgQueue = new PQueue({ concurrency: 1 });
    this.log(`Creating session, language ${this.lang}`);
  }

  run = async (args, options) => {
    return await util.run(args, this.log, options);
  };

  privilegedSession = () => util.privilegedSession(this.context);
  privilegedExec = (cmdline) =>
    util.privilegedExec(this.context, bash(cmdline));
  privilegedPty = (cmdline) =>
    util.privilegedPty(this.context, bash(cmdline, { stty: true }));
  privilegedTeardown = (_cmdline) => util.privilegedTeardown(this.context);

  setup = async () => {
    try {
      setTimeout(this.teardown, 3600 * 1000); // max session length of 1hr
      allSessions.add(this);
      this.send({
        event: "langConfig",
        config: this.config,
      });
      const containerArgs = this.privilegedSession();
      const containerProc = spawn(containerArgs[0], containerArgs.slice(1));
      this.container = {
        proc: containerProc,
      };
      containerProc.on("close", async (code, signal) => {
        this.send({
          event: "serviceFailed",
          service: "container",
          error: `Exited with status ${signal || code}`,
          code: signal || code,
        });
        await this.teardown();
      });
      containerProc.on("error", (err) =>
        this.send({
          event: "serviceFailed",
          service: "container",
          error: `${err}`,
        })
      );
      containerProc.stderr.on("data", (data) =>
        this.send({
          event: "serviceLog",
          service: "container",
          output: data.toString("utf8"),
        })
      );
      let buffer = "";
      await new Promise((resolve) => {
        containerProc.stdout.on("data", (data) => {
          buffer += data.toString();
          let idx;
          while ((idx = buffer.indexOf("\n")) !== -1) {
            const line = buffer.slice(0, idx);
            buffer = buffer.slice(idx + 1);
            if (line === "riju: container ready") {
              resolve();
            } else {
              this.send({
                event: "serviceLog",
                service: "container",
                output: line + "\n",
              });
            }
          }
        });
      });
      if (this.config.setup) {
        await this.run(this.privilegedExec(this.config.setup));
      }
      await this.runCode();
      if (this.config.daemon) {
        const daemonArgs = this.privilegedExec(this.config.daemon);
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
          daemonProc.on("close", (code, signal) =>
            this.send({
              event: "serviceFailed",
              service: "daemon",
              error: `Exited with status ${signal || code}`,
              code: signal || code,
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
      this.ws.on("message", (msg) =>
        this.msgQueue.add(() => this.receive(msg))
      );
      this.ws.on("close", async () => {
        await this.teardown();
      });
      this.ws.on("error", async (err) => {
        logError(err);
        await this.teardown();
      });
      // User may have closed websocket before we were able to set up
      // the onclose listener.
      if (this.ws.readyState === WebSocket.CLOSED) {
        await this.teardown();
      }
    } catch (err) {
      logError(err);
      this.sendError(err);
      await this.teardown();
    }
  };

  send = async (msg) => {
    try {
      if (this.tearingDown) {
        return;
      }
      this.ws.send(JSON.stringify(msg));
    } catch (err) {
      if (this.ws.readyState === WebSocket.CLOSED) {
        // User closed their end, no big deal.
        return;
      }
      logError(err);
      await this.teardown();
    }
  };

  sendError = async (err) => {
    await this.send({ event: "terminalClear" });
    await this.send({
      event: "terminalOutput",
      output: `Riju encountered an unexpected error: ${err}
\r
\rYou may want to save your code and refresh the page.
`,
    });
  };

  logBadMessage = (msg) => {
    this.log(`Got malformed message from client: ${JSON.stringify(msg)}`);
  };

  receive = async (event) => {
    try {
      if (this.tearingDown) {
        return;
      }
      let msg;
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
          this.term.pty.stdin.write(msg.input);
          break;
        case "runCode":
          if (typeof msg.code !== "string") {
            this.logBadMessage(msg);
            break;
          }
          await this.runCode(msg.code);
          break;
        case "testCode":
          if (typeof msg.code !== "string") {
            this.logBadMessage(msg);
            break;
          }
          await this.runCode(msg.code, msg.expectedOutput);
          this.term.pty.stdin.write(TEST_RUN_FINISHED);
          break;
        case "formatCode":
          if (typeof msg.code !== "string") {
            this.logBadMessage(msg);
            break;
          }
          await this.formatCode(msg.code);
          break;
        case "lspStart":
          await this.startLSP();
          break;
        case "lspStop":
          await this.stopLSP();
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
        case "ensure":
          if (!this.config.ensure) {
            this.log(`ensure ignored because of missing configuration`);
            break;
          }
          await this.ensure(this.config.ensure);
          break;
        default:
          this.logBadMessage(msg);
          break;
      }
    } catch (err) {
      logError(err);
      this.sendError(err);
    }
  };

  writeCode = async (code) => {
    if (this.config.main.includes("/")) {
      const dir = path.dirname(`${this.homedir}/${this.config.main}`);
      await this.run(this.privilegedExec(`mkdir -p ${dir}`));
    }
    const file = path.resolve(this.homedir, this.config.main);
    await this.run(this.privilegedExec(`cat > ${file}`), { input: code });
  };

  runCode = async (code, expectedOutput) => {
    try {
      const { name, repl, suffix, createEmpty, compile, run, template } =
        this.config;
      if (this.term) {
        try {
          process.kill(this.term.pty.pid);
        } catch (err) {
          // process might have already exited
        }
        // Signal to terminalOutput message generator using closure.
        this.term.live = false;
        this.term = null;
      }
      this.send({ event: "terminalClear" });
      let cmdline;
      if (code) {
        cmdline = `set +e; ${run}`;
        if (compile) {
          cmdline = `( ${compile} ) && ( ${run} )`;
        }
      } else if (repl) {
        cmdline = repl;
      } else {
        cmdline = `echo '${name} has no REPL, press Run to see it in action'`;
      }
      if (code === undefined) {
        code = createEmpty !== undefined ? createEmpty : template + "\n";
      }
      if (code && suffix) {
        code += "\n" + suffix + "\n";
      }
      await this.writeCode(code);
      const termArgs = this.privilegedPty(cmdline);
      const term = {
        pty: spawn(termArgs[0], termArgs.slice(1)),
        live: true,
      };
      this.term = term;

      this.term.pty.stdout.on("data", (data) => {
        // Capture term in closure so that we don't keep sending output
        // from the old pty even after it's been killed (see ghci).
        if (term.live) {
          const output = data.toString();

          this.send({
            event: "terminalOutput",
            output,
          });

          if (output.includes(TEST_RUN_FINISHED)) {
            this.send({
              event: "testRunFinished",
              expectedOutput
            })
          }
        }
      });
      this.term.pty.stderr.on("data", (data) => {
        if (term.live) {
          this.send({
            event: "serviceLog",
            service: "pty",
            output: data.toString("utf8"),
          });
        }
      });
      this.term.pty.on("close", (code, signal) => {
        if (term.live) {
          this.send({
            event: "serviceFailed",
            service: "terminal",
            error: `Exited with status ${signal || code}`,
            code: signal || code,
          });
        }
      });
      this.term.pty.on("error", (err) => {
        if (term.live) {
          this.send({
            event: "serviceFailed",
            service: "terminal",
            error: `${err}`,
          });
        }
      });
    } catch (err) {
      logError(err);
      this.sendError(err);
    }
  };

  formatCode = async (code) => {
    try {
      if (!this.config.format) {
        this.log("formatCode ignored because format is null");
        return;
      }
      if (this.formatter) {
        const pid = this.formatter.proc.pid;
        const args = this.privilegedExec(
          `kill -SIGTERM ${pid}; sleep 1; kill -SIGKILL ${pid}`
        );
        spawn(args[0], args.slice(1));
        this.formatter.live = false;
        this.formatter = null;
      }
      const args = this.privilegedExec(this.config.format.run);
      const formatter = {
        proc: spawn(args[0], args.slice(1)),
        live: true,
        input: code,
        output: "",
      };
      formatter.proc.stdin.end(code);
      formatter.proc.stdout.on("data", (data) => {
        if (!formatter.live) return;
        formatter.output += data.toString("utf8");
      });
      formatter.proc.stderr.on("data", (data) => {
        if (!formatter.live) return;
        this.send({
          event: "serviceLog",
          service: "formatter",
          output: data.toString("utf8"),
        });
      });
      formatter.proc.on("close", (code, signal) => {
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
            code: signal || code,
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
      logError(err);
      this.sendError(err);
    }
  };

  stopLSP = async () => {
    try {
      if (this.lsp) {
        this.lsp.stopping = true;
        this.lsp.proc.kill();
        this.lsp = null;
      }
    } catch (err) {
      logError(err);
    }
  };

  startLSP = async () => {
    try {
      if (this.config.lsp) {
        await this.stopLSP();
        if (this.config.lsp.setup) {
          await this.run(this.privilegedExec(this.config.lsp.setup));
        }
        const lspArgs = this.privilegedExec(this.config.lsp.start);
        const lspProc = spawn(lspArgs[0], lspArgs.slice(1));
        const lsp = {
          proc: lspProc,
          reader: new rpc.StreamMessageReader(lspProc.stdout),
          writer: new rpc.StreamMessageWriter(lspProc.stdin),
          live: true,
          stopping: false,
        };
        this.lsp = lsp;
        this.lsp.reader.listen((data) => {
          this.send({ event: "lspOutput", output: data });
        });
        lspProc.stderr.on("data", (data) => {
          if (lsp.live) {
            this.send({
              event: "serviceLog",
              service: "lsp",
              output: data.toString("utf8"),
            });
          }
        });
        lspProc.on("close", (code, signal) => {
          if (lsp.stopping) {
            this.send({
              event: "lspStopped",
            });
          } else {
            this.send({
              event: "serviceFailed",
              service: "lsp",
              error: `Exited with status ${signal || code}`,
              code: signal || code,
            });
          }
        });
        lspProc.on("error", (err) =>
          this.send({ event: "serviceFailed", service: "lsp", error: `${err}` })
        );
        this.send({ event: "lspStarted", root: this.homedir });
      }
    } catch (err) {
      logError(err);
    }
  };

  ensure = async (cmd) => {
    try {
      const code = (
        await this.run(this.privilegedExec(cmd), {
          check: false,
        })
      ).code;
      this.send({ event: "ensured", code });
    } catch (err) {
      logError(err);
    }
  };

  teardown = async () => {
    try {
      if (this.tearingDown) {
        return;
      }
      this.log(`Tearing down session`);
      this.tearingDown = true;
      if (this.container) {
        this.container.proc.kill();
      }
      await this.run(this.privilegedTeardown());
      allSessions.delete(this);
      this.ws.terminate();
    } catch (err) {
      logError(err);
    }
  };
}
