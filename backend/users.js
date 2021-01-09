import { spawn } from "child_process";
import { promises as fs } from "fs";
import os from "os";

import AsyncLock from "async-lock";
import _ from "lodash";
import parsePasswd from "parse-passwd";

import { asBool, privilegedUseradd, run, uuidRegexp } from "./util.js";

// Keep in sync with system/src/riju-system-privileged.c
export const MIN_UID = 2000;
export const MAX_UID = 65000;

function validUID(uid) {
  return uid >= MIN_UID && uid < MAX_UID;
}

const CUR_UID = os.userInfo().uid;
const ASSUME_SINGLE_PROCESS = asBool(
  process.env.RIJU_ASSUME_SINGLE_PROCESS,
  false
);

let initialized = false;
let nextUserToCreate = null;
let locallyBorrowedUsers = new Set();
let availableUsers = new Set();
let lock = new AsyncLock();

async function getCreatedUsers() {
  return new Set(
    parsePasswd(await fs.readFile("/etc/passwd", "utf-8"))
      .map(({ uid }) => parseInt(uid))
      .filter((uid) => !isNaN(uid) && validUID(uid))
  );
}

async function getActiveUsers() {
  let dirents;
  try {
    dirents = await fs.readdir("/tmp/riju");
  } catch (err) {
    if (err.code === "ENOENT") {
      return new Set();
    }
    throw err;
  }
  return new Set(
    (
      await Promise.all(
        dirents
          .filter((name) => name.match(uuidRegexp))
          .map((name) => fs.stat(`/tmp/riju/${name}`))
      )
    )
      .map(({ uid }) => uid)
      .filter(validUID)
  );
}

async function createUser(log) {
  if (nextUserToCreate >= MAX_UID) {
    throw new Error("too many users");
  }
  const uid = nextUserToCreate;
  await run(privilegedUseradd(uid), log);
  nextUserToCreate += 1;
  return uid;
}

export async function borrowUser(log) {
  return await lock.acquire("key", async () => {
    if (!initialized || !ASSUME_SINGLE_PROCESS) {
      const createdUsers = await getCreatedUsers();
      const activeUsers = await getActiveUsers();
      if (createdUsers.size > 0) {
        nextUserToCreate = _.max([...createdUsers]) + 1;
      } else {
        nextUserToCreate = MIN_UID;
      }
      // If there are new users created, we want to make them
      // available (unless they are already active). Similarly, if
      // there are users that have become inactive, we want to make
      // them available (unless they are already borrowed locally).
      for (const user of createdUsers) {
        if (!activeUsers.has(user) && !locallyBorrowedUsers.has(user)) {
          availableUsers.add(user);
        }
      }
      // If there are users that have become active, we want to make
      // them unavailable.
      for (const user of activeUsers) {
        availableUsers.delete(user);
      }
      initialized = true;
    }
    if (availableUsers.size === 0) {
      availableUsers.add(await createUser(log));
    }
    // https://stackoverflow.com/a/32539929/3538165
    const user = availableUsers.values().next().value;
    locallyBorrowedUsers.add(user);
    availableUsers.delete(user);
    return {
      uid: user,
      returnUser: async () => {
        await lock.acquire("key", () => {
          locallyBorrowedUsers.delete(user);
          availableUsers.add(user);
        });
      },
    };
  });
}
