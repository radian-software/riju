import { promises as fs } from "fs";

import { getLangs } from "./config.js";

// Parse command-line arguments, run main functionality, and exit.
async function main() {
  await fs.mkdir("docker/composite", { recursive: true });
  const langs = await getLangs();
  // await fs.writeFile("docker/composite/Dockerfile", )
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
