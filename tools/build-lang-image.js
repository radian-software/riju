import crypto from "crypto";
import { promises as fs } from "fs";
import http from "http";
import url from "url";

import { Command } from "commander";
import express from "express";

import { getSharedDepsForLangConfig, readLangConfig } from "../lib/yaml.js";
import { getLocalImageLabel } from "./docker-util.js";
import { hashDockerfile } from "./hash-dockerfile.js";
import { getDebHash, runCommand } from "./util.js";

// Get a Node.js http server object that will allow the Docker
// build to fetch files from outside the container, without them
// being in the build context.
function getServer() {
  const app = express();
  app.use("/fs", express.static("."));
  return http.createServer(app);
}

// Parse command-line arguments, run main functionality, and exit.
async function main() {
  const program = new Command();
  program.requiredOption("--lang <id>", "language ID");
  program.option("--debug", "interactive debugging");
  program.parse(process.argv);
  const { lang, debug } = program.opts();
  const sharedDeps = await getSharedDepsForLangConfig(await readLangConfig(lang));
  const installContents = await fs.readFile(
    `build/lang/${lang}/install.bash`,
    "utf-8"
  );
  const sharedInstallContents = await Promise.all(sharedDeps.map(
    async (name) => fs.readFile(`build/shared/${name}/install.bash`),
  ));
  const allInstallContents = [].concat.apply([installContents], sharedInstallContents);
  const hash = await hashDockerfile(
    "lang",
    {
      "riju:base": await getLocalImageLabel("riju:base", "riju.image-hash"),
    },
    {
      salt: {
        langHash: await getDebHash(`build/lang/${lang}/riju-lang-${lang}.deb`),
        sharedHashes: (
          await Promise.all(
            sharedDeps.map(
              async (name) =>
                await getDebHash(`build/shared/${name}/riju-shared-${name}.deb`)
            )
          )
        ).sort(),
        installHash: allInstallContents.map(
          (c) => crypto.createHash("sha1").update(c).digest("hex"),
        ).join(""),
      },
    }
  );
  const server = getServer();
  await new Promise((resolve) => server.listen(8487, "localhost", resolve));
  try {
    if (debug) {
      await runCommand(
        `docker run -it --rm -e LANG=${lang} -w /tmp/riju-work --network host base:runtime`
      );
    } else {
      await runCommand(
        `docker build . -f docker/lang/Dockerfile ` +
          `--build-arg LANG=${lang} -t riju:lang-${lang} ` +
          `--network host --no-cache --label riju.image-hash=${hash}`
      );
    }
  } finally {
    await server.close();
  }
  process.exit(0);
}

if (process.argv[1] === url.fileURLToPath(import.meta.url)) {
  main().catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
