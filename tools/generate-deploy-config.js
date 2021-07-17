import { promises as fs } from "fs";
import url from "url";

import { Command } from "commander";

import { getLangs } from "../lib/yaml.js";
import { getDockerRepo, getRemoteImageLabel } from "./docker-util.js";

// Get the contents of the JSON file that will be written to S3 in
// order to deploy Riju.
async function getDeployConfig() {
  const DOCKER_REPO = getDockerRepo();
  const langs = await getLangs();
  const langImageTags = Object.fromEntries(
    await Promise.all(
      langs.map(async (lang) => {
        const hash = await getRemoteImageLabel(
          `${DOCKER_REPO}:lang-${lang}`,
          "riju.image-hash"
        );
        if (!hash) {
          throw new Error(`image is not published to ${DOCKER_REPO}: riju:lang-${lang}`);
        }
        return [lang, `lang-${lang}-` + hash];
      })
    )
  );
  const hash = await getRemoteImageLabel(`${DOCKER_REPO}:app`, "riju.image-hash");
  if (!hash) {
    throw new Error(`image is not published to ${DOCKER_REPO}: riju:app`);
  }
  const appImageTag = `app-` + hash;
  return {
    appImageTag,
    langImageTags,
  };
}

// Parse command-line arguments, run main functionality, and exit.
async function main() {
  const program = new Command();
  program.parse(process.argv);
  await fs.mkdir("build", { recursive: true });
  await fs.writeFile(
    "build/config.json",
    JSON.stringify(await getDeployConfig(), null, 2) + "\n"
  );
  console.log("wrote build/config.json");
  process.exit(0);
}

if (process.argv[1] === url.fileURLToPath(import.meta.url)) {
  main().catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
