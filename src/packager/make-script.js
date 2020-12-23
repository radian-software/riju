import process from "process";

import { readLangConfig } from "../config.js";

// Given a language config object, return the text of a Bash script
// that will build the (unpacked) Debian package into ${pkg} when run in an
// appropriate environment.
function makeScript(langConfig) {
  const {
    id,
    name,
    install: { apt, pip, manual },
  } = langConfig;
  const timestamp = new Date().getTime();
  let parts = [];
  parts.push(`\
#!/usr/bin/env bash

set -euxo pipefail`);
  let debianControlData = `\
Package: riju-lang-${id}
Version: ${timestamp}
Architecture: amd64
Maintainer: Radon Rosborough <radon.neon@gmail.com>
Description: The ${name} language packaged for Riju`;
  if (apt.length > 0) {
    debianControlData += `\
Depends: ${apt.join(", ")}`;
  }
  parts.push(`\
install -d "\${pkg}/DEBIAN"
cat <<"EOF" > "\${pkg}/DEBIAN/control"
${debianControlData}
EOF`);
  for (const part of manual || []) {
    parts.push(part.trim());
  }
  return parts.join("\n\n");
}

async function main() {
  const args = process.argv.slice(2);
  if (args.length !== 1) {
    console.error("usage: script-maker.js LANG");
    process.exit(1);
  }
  const [lang] = args;
  console.log(makeScript(await readLangConfig(lang)));
  process.exit(0);
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
