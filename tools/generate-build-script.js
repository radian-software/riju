import nodePath from "path";
import process from "process";
import url from "url";

import { Command } from "commander";
import YAML from "yaml";

import { readLangConfig, readSharedDepConfig } from "../lib/yaml.js";

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
  let depends = [];
  const dependsCfg = (install && install.depends) || {};
  if (
    install &&
    ((install.prepare &&
      ((install.prepare.manual && install.prepare.manual.includes("apt-get")) ||
        (install.prepare.apt && install.prepare.apt.length > 0))) ||
      (install.apt &&
        install.apt.filter((pkg) => pkg.includes("$")).length > 0))
  ) {
    parts.push(`\
export DEBIAN_FRONTEND=noninteractive
sudo --preserve-env=DEBIAN_FRONTEND apt-get update`);
  }
  if (install) {
    const {
      prepare,
      apt,
      riju,
      npm,
      pip,
      gem,
      cpan,
      opam,
      files,
      scripts,
      manual,
      deb,
    } = install;
    if (prepare) {
      const { apt, npm, opam, manual } = prepare;
      if (apt && apt.length > 0) {
        parts.push(`\
sudo --preserve-env=DEBIAN_FRONTEND apt-get install -y ${apt.join(" ")}`);
      }
      if (npm && npm.length > 0) {
        parts.push(`\
sudo npm install -g ${npm.join(" ")}`);
      }
      if (opam && opam.length > 0) {
        parts.push(`\
sudo opam init -n --disable-sandboxing --root /opt/opam
sudo opam install "${opam.join(" ")}" -y --root /opt/opam
sudo ln -s /opt/opam/default/bin/* /usr/local/bin/`);
      }
      if (manual) {
        parts.push(manual);
      }
    }
    if (npm && npm.length > 0) {
      depends.push("nodejs");
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
    if (pip && pip.length > 0) {
      depends.push("python3");
      for (const basename of pip) {
        parts.push(`\
install -d "\${pkg}/usr/local/bin"
pip3 install "${basename}" --prefix "\${pkg}/opt/${basename}"
find "\${pkg}/opt/${basename}" -name __pycache__ -exec rm -rf '{}' ';' -prune

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
    if (gem && gem.length > 0) {
      depends.push("ruby");
      for (const name of gem) {
        parts.push(`\
install -d "\${pkg}/usr/local/bin"
gem install "${name}" -i "/opt/${name}" -n "/opt/${name}/bin" --build-root "\${pkg}"

if [[ -d "\${pkg}/opt/${name}/bin" ]]; then
    (
        set +e
        ls "\${pkg}/opt/${name}/gems/${name}"-*/bin
        ls "\${pkg}/opt/${name}/gems/${name}"-*/exe
        true
    ) | while read name; do
        if [[ -x "\${pkg}/opt/${name}/bin/\${name}" ]]; then
            cat <<EOF > "\${pkg}/usr/local/bin/\${name}"
#!/usr/bin/env bash
exec env GEM_PATH="/opt/${name}" "/opt/${name}/bin/\${name}" "\\\$@"
EOF
            chmod +x "\${pkg}/usr/local/bin/\${name}"
        fi
    done
fi`);
      }
    }
    if (cpan && cpan.length > 0) {
      depends.push("perl");
      for (const fullname of cpan) {
        const basename = fullname.replace(/:+/g, "-").toLowerCase();
        parts.push(`\
install -d "\${pkg}/usr/local/bin"
cpanm -l "\${pkg}/opt/${basename}" -n "${fullname}"

if [[ -d "\${pkg}/opt/${basename}/bin" ]]; then
    ls "\${pkg}/opt/${basename}/bin" | (grep -v config_data || true) | while read name; do
        version="$(ls "\${pkg}/opt/${basename}/lib" | head -n1)"
        cat <<EOF > "\${pkg}/usr/local/bin/\${name}"
#!/usr/bin/env bash
exec env PERL5LIB="/opt/${basename}/lib/\${version}" "/opt/${basename}/bin/\${name}" "\\\$@"
EOF
        chmod +x "\${pkg}/usr/local/bin/\${name}"
    done
fi`);
      }
    }
    if (opam && opam.length > 0) {
      depends.push("ocaml-nox");
      for (let opts of opam) {
        if (typeof opts === "string") {
          opts = { name: opts, binaries: [opts] };
        }
        const { name, source, binaries } = opts;
        let installCmd;
        if (source) {
          installCmd = `opam pin add "${name}" "${source}" -y --root "\${pkg}/opt/${name}"`;
        } else {
          installCmd = `opam install "${name}" -y --root "\${pkg}/opt/${name}"`;
        }
        parts.push(`\
install -d "\${pkg}/usr/local/bin"

opam init -n --disable-sandboxing --root "\${pkg}/opt/${name}"
${installCmd}`);
        parts.push(
          binaries
            .map(
              (binary) =>
                `ln -s "/opt/${name}/default/bin/${binary}" "\${pkg}/usr/local/bin/"`
            )
            .join("\n")
        );
      }
    }
    if (files) {
      for (const [file, contents] of Object.entries(files)) {
        const path = "${pkg}" + file;
        parts.push(`install -d "${nodePath.dirname(path)}"
cat <<"RIJU-EOF" > "${path}"
${contents}
RIJU-EOF`);
      }
    }
    if (scripts) {
      for (const [script, contents] of Object.entries(scripts)) {
        const path = "${pkg}" + nodePath.resolve("/usr/local/bin", script);
        parts.push(`install -d "${nodePath.dirname(path)}"
cat <<"RIJU-EOF" > "${path}"
${contents}
RIJU-EOF
chmod +x "${path}"`);
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
    if (apt) {
      depends = depends.concat(apt);
    }
    if (dependsCfg.unpin) {
      depends = depends.concat(dependsCfg.unpin);
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
  let stripDependsFilter = "";
  const stripDepends = (dependsCfg.strip || []).concat(dependsCfg.unpin || []);
  if (stripDepends.length > 0) {
    stripDependsFilter = ` | sed -E 's/\\{(${stripDepends.join(
      "|"
    )})[^}]*\\}//g'`;
  }
  let debianControlData = `\
Package: riju-${isShared ? "shared" : "lang"}-${id}
Version: \$(date +%s%3N)
Architecture: amd64
Maintainer: Radon Rosborough <radon.neon@gmail.com>
Description: The ${name} ${
    isShared ? "shared dependency" : "language"
  } packaged for Riju
Depends: \$(IFS=,; echo "\${depends[*]}" | sed -E 's/^[ ,]*|[ ,]*$| *(, *)+/},{/g' | sed -E 's/ *(\\| *)+/}\\|{/g'${stripDependsFilter} | tr -d '{}' | sed -E 's/^[,|]+|[,|]+$//g' | sed -E 's/[,|]*,[,|]*/,/g' | sed -E 's/\\|+/|/g')
Riju-Script-Hash: \$(sha1sum "\$0" | awk '{ print \$1 }')`;
  parts.push(`\
install -d "\${pkg}/DEBIAN"
cat <<EOF > "\${pkg}/DEBIAN/control"
${debianControlData}
EOF`);
  if (parts.join("\n\n").includes("latest_release")) {
    parts.unshift(`\
latest_release() {
    curl -sSL "https://api.github.com/repos/\$1/releases/latest" | jq -r .tag_name
}`);
  }
  if (install && install.disallowCI) {
    parts.unshift(`\
if [[ -n "\${CI:-}" ]]; then
    echo "language ${id} cannot be built in CI" >&2
    exit 1
fi`);
  }
  parts.unshift(`\
#!/usr/bin/env bash

set -euxo pipefail`);
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

export async function generateBuildScript({ lang, type }) {
  const scriptMaker = {
    lang: makeLangScript,
    config: makeConfigScript,
    shared: makeSharedScript,
  }[type];
  if (!scriptMaker) {
    throw new Error(`unsupported script type ${type}`);
  }
  return scriptMaker(
    type === "shared"
      ? await readSharedDepConfig(lang)
      : await readLangConfig(lang)
  );
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
  console.log(
    await generateBuildScript({ lang: program.lang, type: program.type })
  );
  process.exit(0);
}

if (process.argv[1] === url.fileURLToPath(import.meta.url)) {
  main().catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
