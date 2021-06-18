import { promises as fs } from "fs";
import url from "url";

// Get the contents of the JSON file that will be written to S3 in
// order to deploy Riju.
async function getDeployConfig() {
  // TODO
}

// Parse command-line arguments, run main functionality, and exit.
async function main() {
  const program = new Command();
  program.parse(process.argv);
  await fs.mkdir("build", { recursive: true });
  await fs.writeFile("build/config.json", JSON.stringify(await getDeployConfig(), null, 2) + "\n");
  process.exit(0);
}

if (process.argv[1] === url.fileURLToPath(import.meta.url)) {
  main().catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
