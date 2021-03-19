// This script is not really needed per se, but it's a bit slow to run
// Make/Node.js several hundred times in order to 'make all-scripts',
// hence having a single script that does the whole thing.

import { promises as fs } from "fs";
import nodePath from "path";
import process from "process";
import url from "url";

import { getPackages } from "../lib/yaml.js";
import { generateBuildScript } from "./generate-build-script.js";

// Parse command-line arguments, run main functionality, and exit.
async function main() {
  for (const { lang, type } of await getPackages()) {
    const scriptPath = `build/${type}/${lang}/build.bash`;
    await fs.mkdir(nodePath.dirname(scriptPath), { recursive: true });
    await fs.writeFile(
      scriptPath,
      (await generateBuildScript({ lang, type })) + "\n"
    );
    await fs.chmod(scriptPath, 0o755);
  }
  process.exit(0);
}

if (process.argv[1] === url.fileURLToPath(import.meta.url)) {
  main().catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
