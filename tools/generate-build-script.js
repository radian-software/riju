import nodePath from "path";
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
  const { id, name, install } = langConfig;
  let parts = [];
  parts.push(`\
#!/usr/bin/env bash

set -euxo pipefail`);
  let depends = [];
  if (install) {
    const { prepare, apt, riju, npm, pip, deb, scripts, manual } = install;
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
    if (npm) {
      for (let fullname of npm) {
        let arg;
        if (typeof fullname === "string") {
          arg = fullname;
        } else {
          arg = fullname.arg;
          fullname = fullname.name;
        }
        let basename = fullname.replace(/^[^\/]+\//g, "");
        parts.push(`\
install -d "\${pkg}/usr/local/bin"
install -d "\${pkg}/opt/${basename}/lib"
npm install ${arg} -g --prefix "\${pkg}/opt/${basename}"
if [[ -d "$\{pkg}/opt/${basename}/bin" ]]; then
    ls "$\{pkg}/opt/${basename}/bin" | while read name; do
        if readlink "\${pkg}/opt/${basename}/bin/\${name}" | grep -q '/${fullname}/'; then
            ln -s "/opt/${basename}/bin/\${name}" "\${pkg}/usr/local/bin/\${name}"
        fi
    done
fi`);
      }
    }
    if (pip) {
      for (const basename of pip) {
        parts.push(`\
install -d "\${pkg}/usr/local/bin"
pip3 install "${basename}" --prefix "\${pkg}/opt/${basename}"
if [[ -d "\${pkg}/opt/${basename}/bin" ]]; then
    ls "\${pkg}/opt/${basename}/bin" | while read name; do
        version="$(ls "\${pkg}/opt/${basename}/lib" | head -n1)"
        cat <<EOF > "\${pkg}/usr/local/bin/\${name}"
#!/usr/bin/env bash
exec env PYTHONPATH="/opt/${basename}/lib/\${version}/site-packages" "/opt/${basename}/bin/\${name}" "\\\$@"
EOF
        chmod +x "\${pkg}/usr/local/bin/\${name}"
    done
fi

if [[ -d "\${pkg}/opt/${basename}/man" ]]; then
    ls "\${pkg}/opt/${basename}/man" | while read dir; do
        install -d "\${pkg}/usr/local/man/\${dir}"
        ls "\${pkg}/opt/${basename}/man/\${dir}" | while read name; do
            ln -s "/opt/${basename}/man/\${dir}/\${name}" "\${pkg}/usr/local/man/\${dir}/\${name}"
        done
    done
fi`);
      }
    }
    if (deb) {
      parts.push(
        deb.map((deb) => `dpkg-deb --extract "${deb}" "\${pkg}"`).join("\n")
      );
    }
    if (scripts) {
      for (const [script, contents] of Object.entries(scripts)) {
        const path = "${pkg}" + nodePath.resolve("/usr/local/bin", script);
        parts.push(`install -d "\${pkg}/usr/local/bin"
cat <<"RIJU-EOF" > "${path}"
${contents}
RIJU-EOF
chmod +x "${path}"`);
      }
    }
    if (manual) {
      parts.push(manual);
    }
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
