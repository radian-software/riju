import { spawn } from "child_process";
import { promises as fs } from "fs";
import process from "process";

import { quote } from "shell-quote";
import { v4 as getUUID } from "uuid";

import { borrowUser } from "./users.js";
import {
  privilegedSetup,
  privilegedSpawn,
  privilegedTeardown,
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
  const uuid = getUUID();
  const { uid, returnUser } = await borrowUser(log);
  await run(privilegedSetup({ uid, uuid }), log);
  const args = privilegedSpawn({ uid, uuid }, [
    "bash",
    "-c",
    `exec env L='${lang}' bash --rcfile <(cat <<< ${quote([sandboxScript])})`,
  ]);
  const proc = spawn(args[0], args.slice(1), {
    stdio: "inherit",
  });
  await new Promise((resolve, reject) => {
    proc.on("error", reject);
    proc.on("close", resolve);
  });
  await run(privilegedTeardown({ uid, uuid }), log);
  await returnUser();
}

main().catch(die);
