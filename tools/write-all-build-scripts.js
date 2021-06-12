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
    const buildScriptPath = `build/${type}/${lang}/build.bash`;
    const installScriptPath = `build/${type}/${lang}/install.bash`;
    await fs.mkdir(nodePath.dirname(buildScriptPath), { recursive: true });
    await fs.writeFile(
      buildScriptPath,
      (await generateBuildScript({ lang, type })) + "\n"
    );
    await fs.chmod(buildScriptPath, 0o755);
    if (type === "lang") {
      await fs.writeFile(
        installScriptPath,
        (await generateBuildScript({ lang, type: "install" })) + "\n"
      );
      await fs.chmod(installScriptPath, 0o755);
    }
  }
  process.exit(0);
}

if (process.argv[1] === url.fileURLToPath(import.meta.url)) {
  main().catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
