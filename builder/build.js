const child_process = require("child_process");
const fs = require("fs").promises;
const path = require("path");
const process = require("process");

const tmp = require("tmp-promise");
const YAML = require("yaml");

// The build scripts in the language configs assume a specific build
// environment, with these parameters:
//
// * the working directory starts out empty
// * the ${pkg} environment variable has been set to an absolute path
//   to the directory where the package should be built
// * we are using bash with 'set -euxo pipefail'

// Read the YAML config file for the language with the given string ID
// and return it as an object.
async function readLangConfig(lang) {
  return YAML.parse(await fs.readFile(`langs/${lang}.yaml`, "utf-8"));
}

// Given a shell command as a string, execute it with Bash.
async function runCommand(cmd) {
  return new Promise((resolve, reject) => {
    const proc = child_process.spawn("bash", ["-c", ...cmd]);
    proc.on("error", reject);
    proc.on("close", (code) => {
      if (code === 0) {
        resolve();
      } else {
        reject(new Error(`command exited with code ${code}`));
      }
    });
  });
}

// Given a language config object, assuming that the environment has
// already been set up properly (see top of this file for details),
// run all the commands needed to build a package, and put the
// resulting deb file into debPath.
async function buildPackage(langConfig, debPath) {
  const {
    id,
    name,
    install: { apt, pip, manual },
  } = langConfig;
  const timestamp = new Date().getTime();
  let debianControlData = `\
Package: riju-lang-${id}
Version: ${timestamp}
Architecture: amd64
Maintainer: Radon Rosborough <radon.neon@gmail.com>
Description: The ${name} language packaged for Riju
`;
  if (apt.length > 0) {
    debianControlData += `\
Depends: ${apt.join(", ")}
`;
  }
  await fs.mkdir("DEBIAN");
  await fs.writeFile("DEBIAN/control", debianControlData);
  await runCommand([
    "fakeroot",
    "dpkg-deb",
    "--build",
    process.env.pkg,
    debPath,
  ]);
}

async function main() {
  const args = process.argv.slice(2);
  if (args.length !== 2) {
    console.error("usage: build.js LANG DEB");
    process.exit(1);
  }
  let [lang, debPath] = args;
  debPath = path.join(process.cwd(), debPath);
  const langConfig = await readLangConfig(lang);
  await tmp.withDir(async (o) => {
    const buildDir = o.path;
    await tmp.withDir(async (o) => {
      const pkgDir = o.path;
      process.chdir(buildDir);
      process.env.pkg = pkgDir;
      await buildPackage(langConfig, debPath);
    });
  });
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
