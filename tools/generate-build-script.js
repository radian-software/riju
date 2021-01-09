import process from "process";

import { Command } from "commander";
import YAML from "yaml";

import { readLangConfig, readSharedDepConfig } from "./config.js";

// Given a language config object, return the text of a Bash script
// that will build the (unpacked) riju-lang-foo Debian package into
// ${pkg} when run in an appropriate environment. This is a package
// that will install the language interpreter/compiler and associated
// tools.
//
// isShared (optional) truthy means to generate a shared dependency
// package riju-shared-foo rather than a language installation
// package.
function makeLangScript(langConfig, isShared) {
  const {
    id,
    name,
    install: { prepare, apt, riju, pip, manual, deb },
  } = langConfig;
  let parts = [];
  parts.push(`\
#!/usr/bin/env bash

set -euxo pipefail`);
  if (prepare) {
    const { apt, manual } = prepare;
    if (apt && apt.length > 0) {
      parts.push(`\
export DEBIAN_FRONTEND=noninteractive
sudo apt-get update
sudo apt-get install -y ${apt.join(" ")}`);
    }
    if (manual) {
      parts.push(manual);
    }
  }
  if (manual) {
    parts.push(manual);
  }
  if (deb) {
    parts.push(
      deb.map((deb) => `dpkg-deb --extract "${deb}" "\${pkg}"`).join("\n")
    );
  }
  let depends = [];
  if (apt) {
    depends = depends.concat(apt);
  }
  if (riju) {
    depends = depends.concat(riju.map((name) => `riju-shared-${name}`));
  }
  if (deb) {
    depends = depends.concat(
      deb.map((fname) => `\$(dpkg-deb -f "${fname}" Depends)`)
    );
  }
  parts.push(`depends=(${depends.map((dep) => `"${dep}"`).join(" ")})`);
  let debianControlData = `\
Package: riju-${isShared ? "shared" : "lang"}-${id}
Version: \$(date +%s%3N)
Architecture: amd64
Maintainer: Radon Rosborough <radon.neon@gmail.com>
Description: The ${name} ${
    isShared ? "shared dependency" : "language"
  } packaged for Riju
Depends: \$(IFS=,; echo "\${depends[*]}")
Riju-Script-Hash: \$(sha1sum "\$0" | awk '{ print \$1 }')`;
  parts.push(`\
install -d "\${pkg}/DEBIAN"
cat <<EOF > "\${pkg}/DEBIAN/control"
${debianControlData}
EOF`);
  return parts.join("\n\n");
}

// Given a language config object, return the text of a Bash script
// that will build the (unpacked) riju-config-foo Debian package into
// ${pkg} when run in an appropriate environment. This is a package
// that will install configuration files and/or small scripts that
// encode the language configuration so that Riju can operate on any
// installed languages without knowing their configuration in advance.
function makeConfigScript(langConfig) {
  const { id, name } = langConfig;
  let parts = [];
  parts.push(`\
#!/usr/bin/env bash

set -euxo pipefail`);
  let debianControlData = `\
Package: riju-config-${id}
Version: \$(date +%s%3N)
Architecture: all
Maintainer: Radon Rosborough <radon.neon@gmail.com>
Description: Riju configuration for the ${name} language
Depends: riju-lang-${id}
Riju-Script-Hash: \$(sha1sum "$0" | awk '{ print $1 }')`;
  parts.push(`\
install -d "\${pkg}/DEBIAN"
cat <<EOF > "\${pkg}/DEBIAN/control"
${debianControlData}
EOF`);
  parts.push(`\
install -d "\${pkg}/opt/riju/langs"
cat <<"EOF" > "\${pkg}/opt/riju/langs/${id}.json"
${JSON.stringify(langConfig, null, 2)}
EOF`);
  return parts.join("\n\n");
}

// Given a language config object, return the text of a Bash script
// that will build the (unpacked) riju-shared-foo Debian package into
// ${pkg} when run in an appropriate environment. This is a package
// that installs tools used by multiple languages, and can be declared
// as a dependency.
function makeSharedScript(langConfig) {
  return makeLangScript(langConfig, true);
}

// Parse command-line arguments, run main functionality, and exit.
async function main() {
  const program = new Command();
  program
    .requiredOption("--lang <id>", "language ID")
    .requiredOption(
      "--type <value>",
      "package category (lang, config, shared)"
    );
  program.parse(process.argv);
  const scriptMaker = {
    lang: makeLangScript,
    config: makeConfigScript,
    shared: makeSharedScript,
  }[program.type];
  if (!scriptMaker) {
    console.error(`make-script.js: unsupported --type ${program.type}`);
    process.exit(1);
  }
  console.log(
    scriptMaker(
      program.type === "shared"
        ? await readSharedDepConfig(program.lang)
        : await readLangConfig(program.lang)
    )
  );
  process.exit(0);
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
