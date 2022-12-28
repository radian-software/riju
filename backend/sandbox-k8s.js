import { spawn } from "child_process";
import { promises as fs } from "fs";
import process from "process";

import { readLangConfig } from "../lib/yaml.js";
import * as k8s from "./k8s.js";
import { getUUID, quote } from "./util.js";

function die(msg) {
  console.error(msg);
  process.exit(1);
}

async function main() {
  const sandboxScript = await fs.readFile("backend/sandbox.bash", "utf-8");
  const lang = process.env.L;
  if (!lang) {
    die("environment variable unset: $L");
  }
  const langConfig = await readLangConfig(lang);
  console.log(`Checking for existing sessions`);
  const existingSessions = await k8s.listUserSessions();
  if (existingSessions.length > 0) {
    console.log(`Killing ${existingSessions.length} existing session(s)`);
    await k8s.deleteUserSessions(existingSessions);
  }
  const sessionID = getUUID();
  console.log(`Starting session with UUID ${sessionID}`);
  await k8s.createUserSession({
    sessionID,
    langConfig,
    revisions: {
      agent: "20221228-023645-invisible-amaranth-sparrow",
      ptyify: "20221228-023645-clean-white-gorilla",
    },
  });
  // let buffer = "";
  // await new Promise((resolve) => {
  //   session.stdout.on("data", (data) => {
  //     buffer += data.toString();
  //     let idx;
  //     while ((idx = buffer.indexOf("\n")) !== -1) {
  //       const line = buffer.slice(0, idx);
  //       buffer = buffer.slice(idx + 1);
  //       if (line === "riju: container ready") {
  //         resolve();
  //       } else {
  //         console.error(line);
  //       }
  //     }
  //   });
  // });
  // const args = [].concat.apply(
  //   ["riju-pty", "-f"],
  //   privilegedPty(
  //     { uuid },
  //     bash(
  //       `env L='${lang}' LANG_CONFIG=${quote(
  //         JSON.stringify(langConfig)
  //       )} bash --rcfile <(cat <<< ${quote(sandboxScript)})`
  //     )
  //   )
  // );
  // const proc = spawn(args[0], args.slice(1), {
  //   stdio: "inherit",
  // });
  // try {
  //   await new Promise((resolve, reject) => {
  //     proc.on("error", reject);
  //     proc.on("close", resolve);
  //   });
  // } finally {
  //   session.kill();
  // }
}

main().catch(die);
