// This script is not really needed per se, but it's a bit slow to run
// Make/Node.js several hundred times in order to 'make all-scripts',
// hence having a single script that does the whole thing.

import { promises as fs } from "fs";
import process from "process";
import url from "url";

import { getPackages } from "./config.js";
import { generateBuildScript } from "./generate-build-script.js";

// Parse command-line arguments, run main functionality, and exit.
async function main() {
  for (const { lang, type } of await getPackages()) {
    await fs.mkdir(`build/${type}/${lang}`, { recursive: true });
    await fs.writeFile(
      `build/${type}/${lang}/build.bash`,
      await generateBuildScript({ lang, type })
    );
  }
  process.exit(0);
}

if (process.argv[1] === url.fileURLToPath(import.meta.url)) {
  main().catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
