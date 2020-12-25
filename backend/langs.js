import { promises as fs } from "fs";
import path from "path";

import { log } from "./util.js";

// Map from language IDs to language configuration objects. This is
// populated at runtime and updated asynchronously.
export let langs = {};

async function readLangsFromDisk() {
  const newLangs = {};
  for (const filename of await fs.readdir("/opt/riju/langs")) {
    if (path.parse(filename).ext !== ".json") {
      continue;
    }
    const id = path.parse(filename).name;
    const langConfig = JSON.parse(
      await fs.readFile(`/opt/riju/langs/${filename}`, "utf-8")
    );
    if (langConfig.id !== id) {
      log.error(
        "Language config ${filename} has mismatched language ID ${id}, ignoring"
      );
      continue;
    }
    newLangs[id] = langConfig;
  }
  log.info(
    `Loaded ${Object.keys(newLangs).length} language configuration(s) from disk`
  );
  langs = newLangs;
}

readLangsFromDisk().catch((err) => {
  log.error("Failed to read languages from disk:", err);
});
