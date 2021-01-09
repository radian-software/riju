import { spawn, spawnSync } from "child_process";
import os from "os";
import process from "process";

import { quote } from "shell-quote";

import { MIN_UID, MAX_UID } from "./users.js";

export const rijuSystemPrivileged = "system/out/riju-system-privileged";

const rubyVersion = (() => {
  try {
    return spawnSync("ruby", ["-e", "puts RUBY_VERSION"])
      .stdout.toString()
      .trim();
  } catch (err) {
    return null;
  }
})();

function getEnv({ uid, uuid }) {
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

function getEnvString(ctx) {
  return Object.entries(getEnv(ctx))
    .map(([key, val]) => `${key}=${quote([val])}`)
    .join(" ");
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

export function privilegedUseradd(uid) {
  return [rijuSystemPrivileged, "useradd", `${uid}`];
}

export function privilegedSetup({ uid, uuid }) {
  return [rijuSystemPrivileged, "setup", `${uid}`, uuid];
}

export function privilegedSpawn(ctx, args) {
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

export function privilegedTeardown({ uid, uuid }) {
  return [rijuSystemPrivileged, "teardown", `${uid}`, uuid];
}

export function bash(cmdline) {
  if (!cmdline.match(/[;|&(){}=\n]/)) {
    // Reduce number of subshells we generate, if we're just running a
    // single command (no shell logic).
    cmdline = "exec " + cmdline;
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

// https://gist.github.com/bugventure/f71337e3927c34132b9a
export const uuidRegexp = /^[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}$/;

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
