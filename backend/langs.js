import fsOrig, { promises as fs } from "fs";
import path from "path";

import debounce from "debounce";

import { getLangs, readLangConfig } from "../lib/yaml.js";
import { log } from "./util.js";

// Map from language IDs to language configuration objects. This is
// populated at runtime and updated asynchronously.
export let langs = {};

// Map from language aliases and IDs to canonical language IDs.
export let aliases = {};

// Read languages from YAML, and update the global langs variable in
// this module. Never throw an error. If there is a problem then just
// leave the languages as they previously were.
async function updateLangsFromDisk() {
  try {
    const newLangs = {};
    const newAliases = {};
    for (const langConfig of await Promise.all(
      (await getLangs()).map(readLangConfig)
    )) {
      const { id } = langConfig;
      newLangs[id] = langConfig;
      newAliases[id] = id;
      for (const alias of langConfig.aliases || []) {
        newAliases[alias] = id;
      }
    }
    log.info(
      `Loaded ${
        Object.keys(newLangs).length
      } language configuration(s) from disk`
    );
    langs = newLangs;
    aliases = newAliases;
  } catch (err) {
    log.error("Failed to read languages from disk:", err);
  }
}

export const langsPromise = updateLangsFromDisk().then(() => langs);

export const langWatcher = fsOrig.watch("langs", debounce(updateLangsFromDisk, 200));
