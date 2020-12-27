import crypto from "crypto";
import { promises as fs } from "fs";
import process from "process";

import { getLangs } from "./config.js";
import { runCommand } from "./util.js";

// Parse command-line arguments, run main functionality, and exit.
async function main() {
  const args = process.argv.slice(2);
  if (args.length !== 1) {
    console.error(
      "usage: node hash-composite-image.js (scripts | debs | remote)"
    );
    process.exit(1);
  }
  const mode = args[0];
  let getHash;
  switch (mode) {
    case "scripts":
      getHash = async (lang, type) => {
        const text = await fs.readFile(
          `build/${type}/${lang}/build.bash`,
          "utf-8"
        );
        return crypto.createHash("sha1").update(text).digest("hex");
      };
      break;
    case "debs":
      getHash = async (lang, type) => {
        return (
          await runCommand(
            `dpkg-deb -f build/${type}/${lang}/riju-${type}-${lang}.deb Riju-Script-Hash`,
            { getStdout: true }
          )
        ).stdout.trim();
      };
      break;
    case "s3":
      const remoteHashes = Object.fromEntries(
        (
          await runCommand("tools/list-s3-hashes.bash", { getStdout: true })
        ).stdout
          .trim()
          .split("\n")
          .map((path) => {
            const [_, pkg, hash] = path.split("/");
            return [pkg, hash];
          })
      );
      getHash = async (lang, type) => remoteHashes[`riju-${type}-${lang}`];
      break;
    default:
      console.error(`hash-composite-image.js: unsupported mode: ${mode}`);
      process.exit(1);
  }
  const langs = await getLangs();
  const hashes = {};
  for (const lang of langs) {
    for (const type of ["config", "lang"]) {
      const hash = await getHash(lang, type);
      if (hash.length !== 40) {
        throw new Error(`malformed hash: ${hash}`);
      }
      hashes[`riju-${type}-${lang}`] = hash;
    }
  }
  const allHashes = Object.values(hashes).sort().join(",");
  console.log(crypto.createHash("sha1").update(allHashes).digest("hex"));
  process.exit(0);
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
