const fs = require("fs").promises;
const process = require("process");

const YAML = require("yaml");

async function main() {
  const args = process.argv.slice(2);

  if (args.length !== 1) {
    console.error("usage: debug.js LANG");
    process.exit(1);
  }

  const [lang] = args;

  console.log(
    JSON.stringify(
      YAML.parse(await fs.readFile(`langs/${lang}.yaml`, "utf-8")),
      null,
      2
    )
  );
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
