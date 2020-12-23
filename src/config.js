import _fs from "fs";
const fs = _fs.promises;

import YAML from "yaml";

// The build scripts in the language configs assume a specific build
// environment, with these parameters:
//
// * the working directory starts out empty
// * the ${pkg} environment variable has been set to an absolute path
//   to the directory where the package should be built; this
//   directory also starts out empty
// * we are using bash with 'set -euxo pipefail'

// Read the YAML config file for the language with the given string ID
// and return it as an object.
export async function readLangConfig(lang) {
  const langConfig = YAML.parse(
    await fs.readFile(`langs/${lang}.yaml`, "utf-8")
  );
  if (langConfig.id !== lang) {
    throw new Error(
      `lang config id ${langConfig.id} doesn't match expected ${lang}`
    );
  }
  return langConfig;
}
