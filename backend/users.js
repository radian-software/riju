import { spawn } from "child_process";
import fs from "fs";
import os from "os";

import AsyncLock from "async-lock";
import _ from "lodash";
import parsePasswd from "parse-passwd";

import { privilegedUseradd, run } from "./util.js";

// Keep in sync with system/src/riju-system-privileged.c
export const MIN_UID = 2000;
export const MAX_UID = 65000;

const CUR_UID = os.userInfo().uid;

let availIds = null;
let nextId = null;
let lock = new AsyncLock();

async function readExistingUsers(log) {
  availIds = parsePasswd(
    await new Promise((resolve, reject) =>
      fs.readFile("/etc/passwd", "utf-8", (err, data) => {
        if (err) {
          reject(err);
        } else {
          resolve(data);
        }
      })
    )
  )
    .filter(({ username }) => username.startsWith("riju"))
    .map(({ uid }) => parseInt(uid))
    .filter((uid) => !isNaN(uid) && uid >= MIN_UID && uid < MAX_UID)
    .reverse();
  nextId = (_.max(availIds) || MIN_UID - 1) + 1;
  log(`Found ${availIds.length} existing users, next is riju${nextId}`);
}

async function createUser(log) {
  if (nextId >= MAX_UID) {
    throw new Error("too many users");
  }
  const uid = nextId;
  await run(privilegedUseradd(uid), log);
  log(`Created new user with ID ${uid}`);
  nextId += 1;
  return uid;
}

export async function ignoreUsers(uids, log) {
  await lock.acquire("key", async () => {
    if (availIds === null || nextId === null) {
      await readExistingUsers(log);
    }
    const uidSet = new Set(uids);
    if (uidSet.size > 0) {
      const plural = uidSet.size !== 1 ? "s" : "";
      log(
        `Ignoring user${plural} from open session${plural}: ${Array.from(uidSet)
          .sort()
          .map((uid) => `riju${uid}`)
          .join(", ")}`
      );
    }
    availIds = availIds.filter((uid) => !uidSet.has(uid));
  });
}

export async function borrowUser(log) {
  return await lock.acquire("key", async () => {
    if (availIds === null || nextId === null) {
      await readExistingUsers(log);
    }
    let uid;
    if (availIds.length > 0) {
      uid = availIds.pop();
    } else {
      uid = await createUser(log);
    }
    return {
      uid,
      returnUID: async () => {
        await lock.acquire("key", () => {
          availIds.push(uid);
        });
      },
    };
  });
}
