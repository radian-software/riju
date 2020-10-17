import { SpawnOptions, spawn, spawnSync } from "child_process";
import * as os from "os";
import * as process from "process";

import * as appRoot from "app-root-path";
import { quote } from "shell-quote";

import { PRIVILEGED } from "./config";
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
    PATH: PRIVILEGED ? path.join(":") : process.env.PATH || "",
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
