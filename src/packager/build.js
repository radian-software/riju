const child_process = require("child_process");
const fs = require("fs").promises;
const path = require("path");
const process = require("process");

const YAML = require("yaml");

// The build scripts in the language configs assume a specific build
// environment, with these parameters:
//
// * the working directory starts out empty
// * the ${pkg} environment variable has been set to an absolute path
//   to the directory where the package should be built
// * we are using bash with 'set -euo pipefail'

// Read the YAML config file for the language with the given string ID
// and return it as an object.
async function readLangConfig(lang) {
  const langConfig = YAML.parse(
    await fs.readFile(`langs/${lang}.yaml`, "utf-8")
  );
  if (langConfig.id !== lang) {
    throw new Error(
      `lang config id ${langConfig.id} doesn't match expected ${lang}`
    );
  }
  return langConfig;
}

// Used to log all progress messages. Not sure what this should do
// quite yet.
function log(message) {
  console.error(message ? message.trimEnd() : "");
}

// Given a shell command as a string, execute it with Bash.
async function runCommand(cmd) {
  log(`$ ${cmd}`);
  return new Promise((resolve, reject) => {
    const proc = child_process.spawn(
      "bash",
      ["-c", `set -euo pipefail; ${cmd}`],
      {
        stdio: "inherit",
      }
    );
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
  const pkgdir = process.env.pkg;
  log();
  log(`Building package riju-lang-${id}...`);
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
  log("Writing Debian control file:");
  log(debianControlData.replaceAll(/^/gm, "  "));
  await fs.mkdir(`${pkgdir}/DEBIAN`);
  await fs.writeFile(`${pkgdir}/DEBIAN/control`, debianControlData);
  await runCommand(`fakeroot dpkg-deb --build ${pkgdir} ${debPath}`);
  log(`Finished building package riju-lang-${id}.`);
  log();
}

// Create a temporary directory and call the given sync or async
// function with its path as a string. Once the function returns, make
// sure the directory and its contents are deleted.
async function withTempDir(cb) {
  return await tmp.withDir(
    async (o) => {
      await cb(o.path);
    },
    {
      unsafeCleanup: true,
    }
  );
}

// Parse command line and run main functionality. This changes the
// process environment destructively.
async function main() {
  const args = process.argv.slice(2);
  if (args.length !== 1) {
    console.error("usage: build.js LANG");
    process.exit(1);
  }
  const [lang] = args;
  const cwd = process.cwd();
  const srcdir = `${cwd}/work/${lang}/src`;
  const pkgdir = `${cwd}/work/${lang}/pkg`;
  const debPath = `${cwd}/debs/riju-lang-${lang}.deb`;
  const langConfig = await readLangConfig(lang);
  log(`Source directory: ${srcdir}`);
  log(`Package directory: ${pkgdir}`);
  log(`Will write .deb file to: ${debPath}`);
  await fs.rmdir(srcdir, { recursive: true });
  await fs.rmdir(pkgdir, { recursive: true });
  await fs.rm(debPath, { force: true });
  await fs.mkdir(srcdir, { recursive: true });
  await fs.mkdir(pkgdir, { recursive: true });
  await fs.mkdir(path.dirname(debPath), { recursive: true });
  process.chdir(srcdir);
  process.env.pkg = pkgdir;
  await buildPackage(langConfig, debPath);
}

main().catch((err) => {
  console.error();
  console.error(err);
  process.exit(1);
});
