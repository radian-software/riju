import { spawn } from "child_process";
import * as fs from "fs";

import { v4 as getUUID } from "uuid";

import { langs } from "./langs";
import { borrowUser } from "./users";
import {
  getEnv,
  privilegedSetup,
  privilegedSpawn,
  privilegedTeardown,
  run,
} from "./util";

function die(msg: any) {
  console.error(msg);
  process.exit(1);
}

function log(msg: any) {
  console.log(msg);
}

async function main() {
  const uuid = getUUID();
  const { uid, returnUID } = await borrowUser(log);
  await run(privilegedSetup({ uid, uuid }), log);
  const args = privilegedSpawn({ uid, uuid }, ["bash"]);
  const proc = spawn(args[0], args.slice(1), {
    env: getEnv(uuid),
    stdio: "inherit",
  });
  await new Promise((resolve, reject) => {
    proc.on("error", reject);
    proc.on("exit", resolve);
  });
  await run(privilegedTeardown({ uid, uuid }), log);
  await returnUID();
}

main().catch(die);
