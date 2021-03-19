import process from "process";
import url from "url";

import { getPackages } from "../lib/yaml.js";
import { runCommand } from "./util.js";

// Parse command-line arguments, run main functionality, and exit.
async function main() {
  const args = process.argv.slice(2);
  if (args.length < 2) {
    console.error("usage: make-foreach.js (--pkgs | --types) TARGET...");
    process.exit(1);
  }
  const [selector, ...targets] = args;
  switch (selector) {
    case "--pkgs":
      for (const { lang, type } of await getPackages()) {
        await runCommand(
          `MAKELEVEL= make ${targets.join(" ")} L=${lang} T=${type}`
        );
      }
      break;
    case "--types":
      for (const type of ["lang", "config"]) {
        await runCommand(`MAKELEVEL= make ${targets.join(" ")} T=${type}`);
      }
      break;
    default:
      console.error(`make-foreach.js: unknown selector: ${selector}`);
      process.exit(1);
  }
  process.exit(0);
}

if (process.argv[1] === url.fileURLToPath(import.meta.url)) {
  main().catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
