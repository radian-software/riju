import { promises as fs } from "fs";
import http from "http";

import express from "express";

import { getLangs, getPackages, getSharedDeps } from "./config.js";
import { getLocalImageLabel } from "./docker-util.js";
import { hashDockerfile } from "./hash-dockerfile.js";
import { runCommand } from "./util.js";

// Number of package installation layers in the composite Docker
// image. This needs to match the number of installation RUN commands
// in the composite Dockerfile.
const NUM_SHARDS = 10;

// Get a Node.js http server object that will serve information and
// files for packages that should be installed into the composite
// Docker image.
function getServer({ shards }) {
  const app = express();
  app.get("/shard/:shard", (req, res) => {
    res.send(
      shards[parseInt(req.params.shard)]
        .map(({ debPath }) => debPath + "\n")
        .join("")
    );
  });
  app.use("/fs", express.static("."));
  return http.createServer(app);
}

// Given a list of the packages to be built, split them into shards.
// Return a list of shards. Each shard is a list of the package
// objects, such that there are NUM_SHARDS shards. Traversing each
// shard in order will return the packages in the same order as the
// original list.
//
// Currently this uses an extremely simple algorithm, but that might
// be improved in the future.
function getShards(pkgs) {
  const shards = [];
  for (let i = 0; i < NUM_SHARDS; ++i) {
    shards.push([]);
  }
  const shardSize = Math.ceil(pkgs.length / NUM_SHARDS);
  for (let i = 0; i < pkgs.length; ++i) {
    shards[Math.floor(i / shardSize)].push(pkgs[i]);
  }
  return shards;
}

// Parse command-line arguments, run main functionality, and exit.
async function main() {
  const packages = await getPackages();
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
            packages.map(async ({ debPath }) => {
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
    shards: getShards(packages),
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
