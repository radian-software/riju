import { spawn } from "child_process";
import process from "process";

import * as Sentry from "@sentry/node";
import { v4 as getUUIDOrig } from "uuid";

let sentryEnabled = false;

if (process.env.SENTRY_DSN) {
  Sentry.init({
    dsn: process.env.SENTRY_DSN,
  });
  sentryEnabled = true;
}

export function logError(err) {
  console.error(err);
  if (sentryEnabled) {
    Sentry.captureException(err);
  }
}

function computeImageHashes() {
  let deployConfig = process.env.RIJU_DEPLOY_CONFIG;
  if (!deployConfig) return {};
  deployConfig = JSON.parse(deployConfig);
  const imageHashes = {};
  for (const [lang, tag] of Object.entries(deployConfig.langImageTags)) {
    const prefix = `lang-${lang}-`;
    if (!tag.startsWith(prefix)) {
      throw new Error(`malformed tag ${tag}`);
    }
    const imageHash = tag.slice(prefix.length);
    if (imageHash.length !== 40) {
      throw new Error(`malformed tag ${tag}`);
    }
    imageHashes[lang] = imageHash;
  }
  return imageHashes;
}

const imageHashes = computeImageHashes();

export function quote(str) {
  return "'" + str.replace(/'/g, `'"'"'`) + "'";
}

export const rijuSystemPrivileged = "system/out/riju-system-privileged";

export function getUUID() {
  return getUUIDOrig().replace(/-/g, "");
}

export async function run(args, log, options) {
  options = options || {};
  const input = options.input;
  const check = options.check === undefined ? true : options.check;
  const suppressOutput = options.suppressOutput || false;
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
      if (output && !suppressOutput) {
        log(`Output from ${args[0]}:\n` + output);
      }
      if (code === 0 || !check) {
        resolve({ code, output });
      } else {
        reject(`command ${args[0]} failed with error code ${signal || code}`);
      }
    });
  });
}

export function privilegedList() {
  return [rijuSystemPrivileged, "list"];
}

export function privilegedPull({ repo, tag }) {
  return [rijuSystemPrivileged, "pull", repo, tag];
}

export function privilegedSession({ uuid, lang }) {
  const cmdline = [rijuSystemPrivileged, "session", uuid, lang];
  if (imageHashes[lang]) {
    cmdline.push(imageHashes[lang]);
  }
  return cmdline;
}

export function privilegedExec({ uuid }, args) {
  return [rijuSystemPrivileged, "exec", uuid].concat(args);
}

export function privilegedPty({ uuid }, args) {
  return [rijuSystemPrivileged, "pty", uuid].concat(args);
}

export function privilegedTeardown(options) {
  options = options || {};
  const { uuid } = options;
  const cmdline = [rijuSystemPrivileged, "teardown"];
  if (uuid) {
    cmdline.push(uuid);
  }
  return cmdline;
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
