import { spawn } from "child_process";
import { promises as fs } from "fs";
import process from "process";

import { readLangConfig } from "../lib/yaml.js";
import {
  bash,
  getUUID,
  privilegedExec,
  privilegedPty,
  privilegedSession,
  quote,
  run,
} from "./util.js";

function die(msg) {
  console.error(msg);
  process.exit(1);
}

function log(msg) {
  console.log(msg);
}

async function main() {
  const sandboxScript = await fs.readFile("backend/sandbox.bash", "utf-8");
  const lang = process.env.L;
  if (!lang) {
    die("environment variable unset: $L");
  }
  const langConfig = await readLangConfig(lang);
  const uuid = getUUID();
  console.log(`Starting session with UUID ${uuid}`);
  const sessionArgs = privilegedSession({ uuid, lang });
  const session = spawn(sessionArgs[0], sessionArgs.slice(1), {
    stdio: ["ignore", "pipe", "inherit"],
  });
  let buffer = "";
  await new Promise((resolve) => {
    session.stdout.on("data", (data) => {
      buffer += data.toString();
      let idx;
      while ((idx = buffer.indexOf("\n")) !== -1) {
        const line = buffer.slice(0, idx);
        buffer = buffer.slice(idx + 2);
        if (line === "riju: container ready") {
          resolve();
        } else {
          console.error(line);
        }
      }
    });
  });
  const args = privilegedPty(
    { uuid },
    bash(
      `env L='${lang}' LANG_CONFIG=${quote(
        JSON.stringify(langConfig)
      )} bash --rcfile <(cat <<< ${quote(sandboxScript)})`
    )
  );
  const proc = spawn(args[0], args.slice(1), {
    stdio: "inherit",
  });
  try {
    await new Promise((resolve, reject) => {
      proc.on("error", reject);
      proc.on("close", resolve);
    });
  } finally {
    session.kill();
  }
}

main().catch(die);
