import { promises as fs } from "fs";
import http from "http";

import express from "express";

import { getLangs, getPackages, getSharedDeps } from "./config.js";
import { getLocalImageLabel } from "./docker-util.js";
import { hashDockerfile } from "./hash-dockerfile.js";
import { runCommand } from "./util.js";

// Get a Node.js http server object that will serve information and
// files for packages that should be installed into the composite
// Docker image.
function getServer({ langs, sharedDeps }) {
  const app = express();
  app.get("/langs", (req, res) => {
    res.send(langs.map((lang) => lang + "\n").join(""));
  });
  app.get("/shared", (req, res) => {
    res.send(sharedDeps.map((lang) => lang + "\n").join(""));
  });
  app.use("/fs", express.static("."));
  return http.createServer(app);
}

// Parse command-line arguments, run main functionality, and exit.
async function main() {
  const hash = await hashDockerfile(
    "composite",
    {
      "riju:runtime": await getLocalImageLabel(
        "riju:runtime",
        "riju.image-hash"
      ),
    },
    {
      salt: {
        packageHashes: (
          await Promise.all(
            (await getPackages()).map(async ({ debPath }) => {
              return (
                await runCommand(`dpkg-deb -f ${debPath} Riju-Script-Hash`, {
                  getStdout: true,
                })
              ).stdout.trim();
            })
          )
        ).sort(),
      },
    }
  );
  const server = getServer({
    langs: await getLangs(),
    sharedDeps: await getSharedDeps(),
  });
  await new Promise((resolve) => server.listen(8487, "localhost", resolve));
  try {
    await runCommand(
      `docker build . -f docker/composite/Dockerfile -t riju:composite` +
        ` --network host --no-cache --label riju.image-hash=${hash}`
    );
  } finally {
    await server.close();
  }
  process.exit(0);
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
