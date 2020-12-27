import { promises as fs } from "fs";
import http from "http";

import express from "express";

import { getLangs } from "./config.js";
import { runCommand } from "./util.js";

// Get a Node.js http server object that will serve information and
// files for packages that should be installed into the composite
// Docker image.
function getServer(langs) {
  const app = express();
  app.get("/langs", (req, res) => {
    res.send(langs.map((lang) => lang + "\n").join(""));
  });
  app.use("/fs", express.static("."));
  return http.createServer(app);
}

// Parse command-line arguments, run main functionality, and exit.
async function main() {
  const server = getServer(await getLangs());
  await new Promise((resolve) => server.listen(8487, "localhost", resolve));
  try {
    await runCommand(
      "docker build . -f docker/composite/Dockerfile -t riju:composite --network host --no-cache"
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
