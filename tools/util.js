import child_process from "child_process";

// Given a shell command as a string, execute it with Bash.
export async function runCommand(cmd) {
  console.error(`$ ${cmd}`);
  return new Promise((resolve, reject) => {
    const proc = child_process.spawn(
      "bash",
      ["-c", `set -euo pipefail; ${cmd}`],
      {
        stdio: "inherit",
      }
    );
    proc.on("error", reject);
    proc.on("close", (code) => {
      if (code === 0) {
        resolve();
      } else {
        reject(new Error(`command exited with code ${code}`));
      }
    });
  });
}
