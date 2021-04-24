import { spawn, spawnSync } from "child_process";
import os from "os";
import process from "process";

import { quote } from "shell-quote";
import { v4 as getUUIDOrig } from "uuid";

export const rijuSystemPrivileged = "system/out/riju-system-privileged";

export function getUUID() {
  return getUUIDOrig().replace(/-/g, "");
}

export async function run(args, log, options) {
  options = options || {};
  const input = options.input;
  const check = options.check === undefined ? true : options.check;
  delete options.input;
  delete options.check;
  const proc = spawn(args[0], args.slice(1), options);
  if (typeof input === "string") {
    proc.stdin.end(input);
  }
  let output = "";
  proc.stdout.on("data", (data) => {
    output += `${data}`;
  });
  proc.stderr.on("data", (data) => {
    output += `${data}`;
  });
  return await new Promise((resolve, reject) => {
    proc.on("error", reject);
    proc.on("close", (code, signal) => {
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

export function privilegedSession({ uuid, lang }) {
  return [rijuSystemPrivileged, "session", uuid, lang];
}

export function privilegedWait({ uuid }) {
  return [rijuSystemPrivileged, "wait", uuid];
}

export function privilegedExec({ uuid }, args) {
  return [rijuSystemPrivileged, "exec", uuid].concat(args);
}

export function privilegedPty({ uuid }, args) {
  return [rijuSystemPrivileged, "pty", uuid].concat(args);
}

export function bash(cmdline, opts) {
  const stty = opts && opts.stty;
  if (!cmdline.match(/[;|&(){}=\n]/)) {
    // Reduce number of subshells we generate, if we're just running a
    // single command (no shell logic).
    cmdline = "exec " + cmdline;
  }
  if (stty) {
    // Workaround https://github.com/moby/moby/issues/25450 (the issue
    // thread claims the bug is resolved and released, but not in my
    // testing).
    cmdline = "stty cols 80 rows 24; " + cmdline;
  }
  return ["bash", "-c", `set -euo pipefail; ${cmdline}`];
}

export const log = {
  trace: console.error,
  debug: console.error,
  info: console.error,
  warn: console.error,
  error: console.error,
};

export function asBool(value, def) {
  if (def === undefined) {
    throw new Error("asBool needs an explicit default value");
  }
  if (!value) {
    return def;
  }
  value = value.toLowerCase().trim();
  if (["y", "yes", "1", "on"].includes(value)) {
    return true;
  }
  if (["n", "no", "0", "off"].includes(value)) {
    return false;
  }
  throw new Error(`asBool doesn't understand value: ${value}`);
}
