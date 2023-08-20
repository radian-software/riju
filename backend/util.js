import { spawn } from "child_process";
import * as fsBase from "fs";
import { promises as fs } from "fs";
import process from "process";

import * as Sentry from "@sentry/node";
import * as tmp from "tmp-promise";
import { v4 as getUUIDOrig } from "uuid";

tmp.setGracefulCleanup();

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

export function quote(str) {
  return "'" + str.replace(/'/g, `'"'"'`) + "'";
}

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

// This function is a little wrapper for riju-pty to help avoid
// reimplementing the C logic in JavaScript, because it was a pain to
// work out the first time. This is done by using named pipes (FIFOs)
// to ferry IPC data back and forth between C and JavaScript.
//
// When calling this function it will create a riju-pty subprocess,
// and invoke your provided handlePtyInput function when the user
// types into the current terminal. It will also return a
// handlePtyOutput function that you can call to cause output to be
// displayed on the current terminal.
//
// Also returned is a handlePtyExit function that you should call when
// the process is supposed to terminate, this will shut down the
// riju-pty subprocess and teardown associated resources.
//
// The point of using this instead of just reading and writing output
// using the normal functions, is that the read/write operations act
// on raw pty control sequences, meaning they can be hooked up to the
// Riju agent control stream from a user session container in the
// cluster.
export function deptyify({ handlePtyInput }) {
  return new Promise((resolve, reject) => {
    let done = false;
    let triggerDone = () => {
      // Calling the function stored in this variable should have the
      // effect of terminating the tmp-promise callback and getting
      // the temporary directory cleaned up.
      done = true;
    };
    tmp
      .withDir(
        async (dir) => {
          const mkfifo = spawn("mkfifo", ["input", "output"], {
            cwd: dir.path,
          });
          await new Promise((resolve, reject) => {
            mkfifo.on("error", reject);
            mkfifo.on("exit", (code) => {
              if (code === 0) {
                resolve();
              } else {
                reject(code);
              }
            });
          });
          const proc = spawn(
            `${process.cwd()}/system/out/riju-pty`,
            // Order is important, stdin can't be read properly from
            // the background without more configuration
            ["-f", "sh", "-c", "cat output & cat > input"],
            {
              cwd: dir.path,
              stdio: "inherit",
            }
          );
          await new Promise((resolve, reject) => {
            proc.on("spawn", resolve);
            proc.on("error", reject);
          });
          proc.on("exit", (_status) => {
            triggerDone();
          });
          const output = await new Promise((resolve, reject) => {
            const timeout = setTimeout(() => reject("timed out"), 5000);
            resolve(fs.open(`${dir.path}/output`, "w"));
            clearTimeout(timeout);
          });
          const input = fsBase.createReadStream(`${dir.path}/input`);
          setTimeout(async () => {
            try {
              for await (const data of input) {
                handlePtyInput(data);
              }
            } catch (err) {
              logError(err);
            }
          }, 0);
          resolve({
            handlePtyOutput: (data) => {
              output.write(data);
            },
            handlePtyExit: async () => {
              try {
                // SIGTERM, wait for proc to exit, if it doesn't,
                // then SIGKILL.
                proc.kill("SIGTERM");
                let timeout = null;
                try {
                  await new Promise((resolve, reject) => {
                    proc.on("exit", resolve);
                    timeout = setTimeout(reject, 250);
                  });
                } catch (err) {
                  proc.kill("SIGKILL");
                } finally {
                  if (timeout) {
                    clearTimeout(timeout);
                  }
                }
              } catch (err) {
                logError(err);
              }
            },
          });
          // Wait before deleting tmpdir...
          await new Promise((resolve) => {
            if (done) {
              resolve();
            } else {
              triggerDone = resolve;
            }
          });
        },
        {
          unsafeCleanup: true,
        }
      )
      .catch((err) => {
        logError(err);
        reject(err);
      });
  });
}
