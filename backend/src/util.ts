import { spawn, SpawnOptions } from "child_process";
import * as os from "os";
import * as process from "process";

import * as appRoot from "app-root-path";
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

function getEnv({ uid, uuid }: Context) {
  const cwd = `/tmp/riju/${uuid}`;
  const path = [
    `${cwd}/.gem/ruby/2.7.0/bin`,
    `${cwd}/.local/bin`,
    `${cwd}/node_modules/.bin`,
    `/usr/local/sbin`,
    `/usr/local/bin`,
    `/usr/sbin`,
    `/usr/bin`,
    `/bin`,
  ];
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
  await new Promise((resolve, reject) => {
    proc.on("error", reject);
    proc.on("close", (code: number) => {
      output = output.trim();
      if (output) {
        log(`Output from ${args[0]}:\n` + output);
      }
      if (code === 0 || !check) {
        resolve();
      } else {
        reject(`command ${args[0]} failed with error code ${code}`);
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
    "bash",
    "-c",
    `exec env -i ${getEnvString(ctx)} "$@"`,
    "--",
  ].concat(args);
}

export function privilegedTeardown({ uid, uuid }: Context) {
  return [rijuSystemPrivileged, "teardown", `${uid}`, uuid];
}

export function bash(cmdline: string) {
  return ["bash", "-c", cmdline];
}
