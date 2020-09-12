import { SpawnOptions, spawn, spawnSync } from "child_process";
import * as os from "os";
import * as process from "process";
import * as nodeReadline from "readline";

import * as appRoot from "app-root-path";
import * as readline from "historic-readline";
import { quote } from "shell-quote";

import { MIN_UID, MAX_UID } from "./users";

export interface Options extends SpawnOptions {
  input?: string;
  check?: boolean;
}

export interface Context {
  uid: number;
  uuid: string;
}

export const rijuSystemPrivileged = appRoot.resolve(
  "system/out/riju-system-privileged"
);

const rubyVersion = (() => {
  try {
    return spawnSync("ruby", ["-e", "puts RUBY_VERSION"])
      .stdout.toString()
      .trim();
  } catch (err) {
    return null;
  }
})();

function getEnv({ uid, uuid }: Context) {
  const cwd = `/tmp/riju/${uuid}`;
  const path = [
    rubyVersion && `${cwd}/.gem/ruby/${rubyVersion}/bin`,
    `${cwd}/.local/bin`,
    `${cwd}/node_modules/.bin`,
    `/usr/local/sbin`,
    `/usr/local/bin`,
    `/usr/sbin`,
    `/usr/bin`,
    `/bin`,
  ].filter((x) => x);
  const username =
    uid >= MIN_UID && uid < MAX_UID ? `riju${uid}` : os.userInfo().username;
  return {
    HOME: cwd,
    HOSTNAME: "riju",
    LANG: process.env.LANG || "",
    LC_ALL: process.env.LC_ALL || "",
    LOGNAME: username,
    PATH: path.join(":"),
    PWD: cwd,
    SHELL: "/usr/bin/bash",
    TERM: "xterm-256color",
    TMPDIR: `${cwd}`,
    USER: username,
    USERNAME: username,
  };
}

function getEnvString(ctx: Context) {
  return Object.entries(getEnv(ctx))
    .map(([key, val]) => `${key}=${quote([val])}`)
    .join(" ");
}

export async function run(
  args: string[],
  log: (msg: string) => void,
  options?: Options
) {
  options = options || {};
  const input = options.input;
  const check = options.check === undefined ? true : options.check;
  delete options.input;
  delete options.check;
  const proc = spawn(args[0], args.slice(1), options);
  if (typeof input === "string") {
    proc.stdin!.end(input);
  }
  let output = "";
  proc.stdout!.on("data", (data: Buffer) => {
    output += `${data}`;
  });
  proc.stderr!.on("data", (data: Buffer) => {
    output += `${data}`;
  });
  return await new Promise((resolve, reject) => {
    proc.on("error", reject);
    proc.on("close", (code: number, signal: string) => {
      output = output.trim();
      if (output) {
        log(`Output from ${args[0]}:\n` + output);
      }
      if (code === 0 || !check) {
        resolve(code);
      } else {
        reject(`command ${args[0]} failed with error code ${signal || code}`);
      }
    });
  });
}

export function privilegedUseradd(uid: number) {
  return [rijuSystemPrivileged, "useradd", `${uid}`];
}

export function privilegedSetup({ uid, uuid }: Context) {
  return [rijuSystemPrivileged, "setup", `${uid}`, uuid];
}

export function privilegedSpawn(ctx: Context, args: string[]) {
  const { uid, uuid } = ctx;
  return [
    rijuSystemPrivileged,
    "spawn",
    `${uid}`,
    uuid,
    "sh",
    "-c",
    `exec env -i ${getEnvString(ctx)} "$@"`,
    "--",
  ].concat(args);
}

export function privilegedTeardown({ uid, uuid }: Context) {
  return [rijuSystemPrivileged, "teardown", `${uid}`, uuid];
}

export function bash(cmdline: string) {
  if (!cmdline.match(/[;|&(){}=]/)) {
    // Reduce number of subshells we generate, if we're just running a
    // single command (no shell logic).
    cmdline = "exec " + cmdline;
  }
  return ["bash", "-c", cmdline];
}

// https://stackoverflow.com/a/10608048/3538165
function fixStreamFor(cli: any, streamName: string) {
  var oldStream = (process as any)[streamName];
  var newStream = Object.create(oldStream);
  newStream.write = function () {
    cli.output.write("\x1b[2K\r");
    var result = oldStream.write.apply(
      this,
      (Array.prototype.slice as any).call(arguments)
    );
    cli._refreshLine();
    return result;
  };
  (process as any).__defineGetter__("old" + streamName, () => oldStream);
  (process as any).__defineGetter__(streamName, () => newStream);
}

export interface ReplOptions {
  onLine: (line: any) => void;
  historyFile: string;
}

export function startRepl(options: ReplOptions) {
  readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    path: appRoot.resolve(options.historyFile),
    next: (cli: nodeReadline.Interface) => {
      fixStreamFor(cli, "stdout");
      fixStreamFor(cli, "stderr");
      cli.setPrompt(">>> ");
      cli.on("line", (line: string) => {
        if (line) {
          options.onLine(line);
        }
        cli.prompt();
      });
      cli.on("SIGINT", () => {
        (process as any).oldstderr.write("^C\n");
        cli.write("", { ctrl: true, name: "u" });
        cli.prompt();
      });
      cli.on("close", () => {
        (process as any).oldstderr.write("^D\n");
        process.exit(0);
      });
      console.log();
      cli.prompt();
    },
  });
}

export function mockSocket() {
  return {
    on: function (type: string, handler: any) {
      switch (type) {
        case "message":
          this.onMessage = handler;
          for (const msg of this.messageReceivedQueue) {
            this.onMessage(msg);
          }
          this.messageReceivedQueue = [];
          break;
        case "send":
          this.send = handler;
          for (const msg of this.messageSentQueue) {
            this.send(msg);
          }
          this.messageSentQueue = [];
          break;
        case "close":
        case "error":
          // No need to clean up, we'll call teardown() explicitly.
          break;
        default:
          throw new Error(`unexpected websocket handler type: ${type}`);
      }
    },
    onMessage: function (msg: any) {
      this.messageReceivedQueue.push(msg);
    },
    send: function (msg: any) {
      this.messageSentQueue.push(msg);
    },
    messageReceivedQueue: [] as any[],
    messageSentQueue: [] as any[],
    terminate: function () {},
  };
}
