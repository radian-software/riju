import { spawn } from "child_process";
import * as fs from "fs";

import { v4 as getUUID } from "uuid";

import { langs } from "./langs";
import { MIN_UID, MAX_UID, borrowUser, ignoreUsers } from "./users";
import {
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
  const dirs = await new Promise<string[]>((resolve, reject) =>
    fs.readdir("/tmp/riju", (err, dirs) => (err ? reject(err) : resolve(dirs)))
  );
  const uids = (
    await Promise.all(
      dirs.map(
        (dir) =>
          new Promise<number>((resolve, reject) =>
            fs.stat(`/tmp/riju/${dir}`, (err, stat) =>
              err ? reject(err) : resolve(stat.uid)
            )
          )
      )
    )
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
