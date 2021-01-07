import { spawn } from "child_process";
import { promises as fs } from "fs";

import { v4 as getUUID } from "uuid";

import { MIN_UID, MAX_UID, borrowUser, ignoreUsers } from "./users.js";
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
  const dirs = await fs.readdir("/tmp/riju");
  const uids = (
    await Promise.all(dirs.map((dir) => fs.stat(`/tmp/riju/${dir}`)))
  ).filter((uid) => uid >= MIN_UID && uid < MAX_UID);
  await ignoreUsers(uids, log);
  const uuid = getUUID();
  const { uid, returnUID } = await borrowUser(log);
  await run(privilegedSetup({ uid, uuid }), log);
  const args = privilegedSpawn({ uid, uuid }, ["bash"]);
  const proc = spawn(args[0], args.slice(1), {
    stdio: "inherit",
  });
  await new Promise((resolve, reject) => {
    proc.on("error", reject);
    proc.on("close", resolve);
  });
  await run(privilegedTeardown({ uid, uuid }), log);
  await returnUID();
}

main().catch(die);
