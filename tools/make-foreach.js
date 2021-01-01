import process from "process";
import url from "url";

import { getPackages } from "./config.js";
import { runCommand } from "./util.js";

// Parse command-line arguments, run main functionality, and exit.
async function main() {
  const targets = process.argv.slice(2);
  if (targets.length === 0) {
    console.error("usage: make-foreach.js TARGET...");
    process.exit(1);
  }
  for (const { lang, type } of await getPackages()) {
    await runCommand(`make ${targets} L=${lang} T=${type}`);
  }
  process.exit(0);
}

if (process.argv[1] === url.fileURLToPath(import.meta.url)) {
  main().catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
