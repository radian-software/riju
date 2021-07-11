import { promises as fs } from "fs";
import path from "path";

import { validate as validateJSONSchema } from "jsonschema";
import _ from "lodash";
import YAML from "yaml";

// The build scripts in the language configs assume a specific build
// environment, with these parameters:
//
// * the working directory starts out empty
// * the ${pkg} environment variable has been set to an absolute path
//   to the directory where the package should be built; this
//   directory also starts out empty
// * we are using bash with 'set -euxo pipefail'

async function readJSONSchemaFromDisk() {
  return YAML.parse(await fs.readFile("lib/jsonschema.yaml", "utf-8"), {
    merge: true,
  });
}

const jsonSchemaPromise = readJSONSchemaFromDisk();

// Return a list of the IDs of all the configured languages. Each such
// ID can be passed to readLangConfig.
export async function getLangs() {
  return (await fs.readdir("langs"))
    .filter((lang) => lang.endsWith(".yaml"))
    .map((lang) => path.parse(lang).name).sort();
}

// Return a list of the IDs of all the configured shared dependencies.
// Each such ID can be passed to readSharedDepConfig.
export async function getSharedDeps() {
  return (await fs.readdir("shared"))
    .filter((lang) => lang.endsWith(".yaml"))
    .map((lang) => path.parse(lang).name);
}

// Return a list of objects representing the packages to be built. See
// the function implementation for the full list of keys.
export async function getPackages() {
  // The order (shared, then lang) is important to get dependencies
  // correct when building artifacts.
  const packages = [];
  for (const lang of await getSharedDeps()) {
    const type = "shared";
    const name = `riju-${type}-${lang}`;
    packages.push({
      lang,
      type,
      name,
      buildScriptPath: `build/${type}/${lang}/build.bash`,
      debPath: `build/${type}/${lang}/${name}.deb`,
    });
  }
  for (const lang of await getLangs()) {
    const type = "lang";
    const name = `riju-${type}-${lang}`;
    packages.push({
      lang,
      type,
      name,
      buildScriptPath: `build/${type}/${lang}/build.bash`,
      debPath: `build/${type}/${lang}/${name}.deb`,
    });
  }
  return packages;
}

// Correct whitespace problems in a language configuration,
// destructively. Return the fixed configuration.
//
// This basically removes trailing whitespace from all values in the
// configuration recursively.
function fixupLangConfig(langConfig) {
  if (typeof langConfig === "string") {
    return langConfig.trimRight();
  } else if (typeof langConfig === "object") {
    for (const key in langConfig) {
      if (langConfig.id === "whitespace" && key === "template") {
        continue;
      }
      langConfig[key] = fixupLangConfig(langConfig[key]);
    }
  }
  return langConfig;
}

// Read the YAML config file for the language with the given string ID
// and return it as an object.
export async function readLangConfig(lang) {
  const langConfig = YAML.parse(
    await fs.readFile(`langs/${lang}.yaml`, "utf-8"),
    { merge: true }
  );
  validateJSONSchema(langConfig, await jsonSchemaPromise, { throwAll: true });
  if (langConfig.id !== lang) {
    throw new Error(
      `lang config id ${langConfig.id} doesn't match expected ${lang}`
    );
  }
  return fixupLangConfig(langConfig);
}

// Read the YAML config file for the shared dependency with the given
// string ID and return it as an object.
export async function readSharedDepConfig(lang) {
  const langConfig = YAML.parse(
    await fs.readFile(`shared/${lang}.yaml`, "utf-8"),
    { merge: true }
  );
  if (langConfig.id !== lang) {
    throw new Error(
      `shared dependency config id ${langConfig.id} doesn't match expected ${lang}`
    );
  }
  return fixupLangConfig(langConfig);
}

// Given a language config JSON, return a list of the Riju shared
// dependency names, or an empty list if none are configured for this
// language. The return value is sorted.
export async function getSharedDepsForLangConfig(langConfig) {
  return [...(langConfig.install && langConfig.install.riju) || []].sort();
}
