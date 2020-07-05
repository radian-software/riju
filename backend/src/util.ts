import { spawn, SpawnOptions } from "child_process";
import * as process from "process";

import * as appRoot from "app-root-path";

interface Options extends SpawnOptions {
  input?: string;
}

export const rijuSystemPrivileged = appRoot.resolve(
  "system/out/riju-system-privileged"
);

export function getEnv(uuid: string) {
  const cwd = `/tmp/riju/${uuid}`;
  return {
    HOME: cwd,
    HOSTNAME: "riju",
    LANG: process.env.LANG || "",
    LC_ALL: process.env.LC_ALL || "",
    PATH: "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/bin",
    PWD: cwd,
    SHELL: "/usr/bin/bash",
    TERM: "xterm-color",
  };
}

export async function call(
  args: string[],
  log: (msg: string) => void,
  options?: Options
) {
  options = options || {};
  const input = options.input;
  delete options.input;
  const proc = spawn(args[0], args.slice(1), options);
  if (input) {
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
      if (code === 0) {
        resolve();
      } else {
        reject(`command ${args[0]} failed with error code ${code}`);
      }
    });
  });
}

export async function callPrivileged(
  args: string[],
  log: (msg: string) => void,
  options?: Options
) {
  await call([rijuSystemPrivileged].concat(args), log, options);
}

export async function spawnPrivileged(
  uid: number,
  uuid: string,
  args: string[],
  log: (msg: string) => void,
  options?: Options
) {
  options = options || {};
  options.env = getEnv(uuid);
  await callPrivileged(
    ["spawn", `${uid}`, `${uuid}`].concat(args),
    log,
    options
  );
}
