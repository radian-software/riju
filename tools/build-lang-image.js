import http from "http";
import url from "url";

import { Command } from "commander";
import express from "express";

import { runCommand } from "./util.js";

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
          `--network host --no-cache`
      );
    }
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
  process.exit(0);
}

if (process.argv[1] === url.fileURLToPath(import.meta.url)) {
  main().catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
