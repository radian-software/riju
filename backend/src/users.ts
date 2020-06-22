import { spawn } from "child_process";
import * as fs from "fs";

import * as AsyncLock from "async-lock";
import * as _ from "lodash";
import * as parsePasswd from "parse-passwd";

import { PRIVILEGED } from "./config";
import { callPrivileged } from "./util";

// Keep in sync with system/src/riju-system-privileged.c
const MIN_UID = 2000;
const MAX_UID = 65000;

const CUR_UID = parseInt(process.env.UID || "") || null;

let availIds: number[] | null = null;
let nextId: number | null = null;
let lock = new AsyncLock();

async function readExistingUsers(log: (msg: string) => void) {
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
    .filter((uid) => !isNaN(uid) && uid >= MIN_UID && uid < MAX_UID);
  nextId = (_.max(availIds) || MIN_UID - 1) + 1;
  log(`Found ${availIds.length} existing users, next ID is ${nextId}`);
}

async function createUser(log: (msg: string) => void): Promise<number> {
  if (nextId! >= MAX_UID) {
    throw new Error("too many users");
  }
  const uid = nextId!;
  await callPrivileged(["useradd", `${uid}`], log);
  log(`Created new user with ID ${uid}`);
  nextId! += 1;
  return uid;
}

export async function borrowUser(log: (msg: string) => void) {
  if (!PRIVILEGED) {
    if (CUR_UID === null) {
      throw new Error("unable to determine current UID");
    } else {
      return { uid: CUR_UID, cleanup: async () => {} };
    }
  } else {
    return await lock.acquire("key", async () => {
      if (availIds === null || nextId === null) {
        await readExistingUsers(log);
      }
      let uid: number;
      if (availIds!.length > 0) {
        uid = availIds!.pop()!;
      } else {
        uid = await createUser(log);
      }
      return {
        uid,
        cleanup: async () => {
          await lock.acquire("key", () => {
            availIds!.push(uid);
          });
        },
      };
    });
  }
}
