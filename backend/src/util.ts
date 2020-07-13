import { spawn, SpawnOptions } from "child_process";
import * as process from "process";

import * as appRoot from "app-root-path";

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

export function getEnv(uuid: string) {
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
  return {
    HOME: cwd,
    HOSTNAME: "riju",
    LANG: process.env.LANG || "",
    LC_ALL: process.env.LC_ALL || "",
    PATH: path.join(":"),
    PWD: cwd,
    SHELL: "/usr/bin/bash",
    TERM: "xterm-color",
  };
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

export function privilegedSpawn({ uid, uuid }: Context, args: string[]) {
  return [rijuSystemPrivileged, "spawn", `${uid}`, uuid].concat(args);
}

export function privilegedTeardown({ uid, uuid }: Context) {
  return [rijuSystemPrivileged, "teardown", `${uid}`, uuid];
}

export function bash(cmdline: string) {
  return ["bash", "-c", cmdline];
}
