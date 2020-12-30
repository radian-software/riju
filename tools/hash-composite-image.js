import crypto from "crypto";
import { promises as fs } from "fs";
import process from "process";
import url from "url";

import { getLangs } from "./config.js";
import { runCommand } from "./util.js";

// Return the composite image hash as a string. This is designed for
// library usage; main() just calls it and prints the result.
//
// If mode is "scripts" then each build script is run through SHA-1
// and the resulting hashes are hashed together.
//
// If mode is "debs" then the Riju-Script-Hash value written into the
// metadata of each .deb (all of them must exist locally) is
// extracted, and they are all hashed together.
//
// If mode is "s3" then all the published hashes are retrieved from
// S3. The relevant ones are hashed together.
//
// If mode is "registry" then the composite Docker image published to
// ${DOCKER_REPO} is inspected to extract its composite hash from an
// image label.
export async function hashCompositeImage(mode) {
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
    case "registry":
      const tags = (
        await runCommand(
          `skopeo list-tags "docker://\${DOCKER_REPO}" | jq -r '.Tags[]'`,
          { getStdout: true }
        )
      ).stdout
        .trim()
        .split("\n");
      if (!tags.includes("composite")) {
        return "not yet published";
      }
      return (
        await runCommand(
          `skopeo inspect docker://raxod502/riju:composite | jq -r '.Labels["riju-composite-hash"]'`,
          { getStdout: true }
        )
      ).stdout.trim();
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
  return crypto.createHash("sha1").update(allHashes).digest("hex");
}

// Parse command-line arguments, run main functionality, and exit.
async function main() {
  const args = process.argv.slice(2);
  if (args.length !== 1) {
    console.error(
      "usage: node hash-composite-image.js (scripts | debs | s3 | registry)"
    );
    process.exit(1);
  }
  const mode = args[0];
  console.log(await hashCompositeImage(mode));
  process.exit(0);
}

if (process.argv[1] === url.fileURLToPath(import.meta.url)) {
  main().catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
