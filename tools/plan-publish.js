import crypto from "crypto";
import { promises as fs } from "fs";
import process from "process";
import url from "url";

import { Command } from "commander";
import _ from "lodash";
import { v4 as getUUID } from "uuid";

import { getPackages } from "./config.js";
import {
  getLocalImageDigest,
  getLocalImageLabel,
  getRemoteImageLabel,
  getDockerRepo,
} from "./docker-util.js";
import { hashDockerfile } from "./hash-dockerfile.js";
import { runCommand } from "./util.js";

async function planDockerImage(name, dependentHashes, opts) {
  const { deps, hashOpts } = opts || {};
  const DOCKER_REPO = getDockerRepo();
  const desired = await hashDockerfile(name, dependentHashes, hashOpts);
  const local = await getLocalImageLabel(`riju:${name}`, "riju.image-hash");
  const remote = await getRemoteImageLabel(
    `${DOCKER_REPO}:${name}`,
    "riju.image-hash"
  );
  dependentHashes[`riju:${name}`] = desired;
  return {
    id: getUUID(),
    deps: deps || [],
    artifact: "Docker image",
    name,
    desired,
    local,
    remote,
    download: async () => {
      if (name === "app") {
        // Magic string parsed by publish.bash
        console.log("[for publish script: plan-publish is tagging app image]");
      }
      await runCommand(`make pull I=${name}`);
    },
    build: async () => {
      if (name === "app") {
        // Magic string parsed by publish.bash
        console.log("[for publish script: plan-publish is tagging app image]");
      }
      await runCommand(`make image I=${name}`);
    },
    upload: async () => {
      if (name === "composite") {
        await runCommand(`make shell I=composite CMD="make test"`);
      }
      await runCommand(`make push I=${name}`);
    },
  };
}

async function planDebianPackages(opts) {
  const { deps } = opts || {};
  const remoteHashes = Object.fromEntries(
    JSON.parse(
      (
        await runCommand(
          `aws s3api list-objects-v2 --bucket riju-debs --prefix hashes`,
          { getStdout: true }
        )
      ).stdout
    ).Contents.map(({ Key: key }) => {
      const [_, remoteName, remoteHash] = key.split("/");
      return [remoteName, remoteHash];
    })
  );
  const packages = await getPackages();
  const langUUIDs = Object.fromEntries(
    packages
      .filter(({ type }) => type === "lang")
      .map(({ lang }) => ["lang", getUUID()])
  );
  return await Promise.all(
    packages.map(async ({ lang, type, name, buildScriptPath, debPath }) => {
      const desired = crypto
        .createHash("sha1")
        .update(await fs.readFile(buildScriptPath, "utf-8"))
        .digest("hex");
      let debExists = true;
      try {
        await fs.access(debPath);
      } catch (err) {
        debExists = false;
      }
      let local = null;
      if (debExists) {
        local =
          (
            await runCommand(`dpkg-deb -f ${debPath} Riju-Script-Hash`, {
              getStdout: true,
            })
          ).stdout.trim() || null;
      }
      const remote = remoteHashes[name] || null;
      return {
        id: getUUID(),
        deps: [
          ...(deps || []),
          type === "config" ? langUUIDs[lang] : getUUID(),
        ],
        artifact: "Debian package",
        name,
        desired,
        local,
        remote,
        download: async () => {
          await runCommand(`make download L=${lang} T=${type}`);
        },
        build: async () => {
          await runCommand(
            `make shell I=packaging CMD="make pkg L=${lang} T=${type}"`
          );
        },
        upload: async () => {
          if (type === "config") {
            await runCommand(
              `make shell I=runtime CMD="make installs test L=${lang}"`
            );
          }
          await runCommand(`make upload L=${lang} T=${type}`);
        },
      };
    })
  );
}

async function computePlan() {
  const dependentHashes = {
    "ubuntu:rolling": await getLocalImageDigest("ubuntu:rolling"),
  };
  const packaging = await planDockerImage("packaging", dependentHashes);
  const runtime = await planDockerImage("runtime", dependentHashes);
  const packages = await planDebianPackages({
    deps: [packaging.id, runtime.id],
  });
  const composite = await planDockerImage("composite", dependentHashes, {
    deps: [runtime.id, ...packages.map(({ id }) => id)],
    hashOpts: {
      salt: {
        packageHashes: packages.map(({ desired }) => desired).sort(),
      },
    },
  });
  const compile = await planDockerImage("compile", dependentHashes);
  const app = await planDockerImage("app", dependentHashes, {
    deps: [composite.id, compile.id],
  });
  return [packaging, runtime, ...packages, composite, compile, app];
}

function printTable(data, headers) {
  const widths = headers.map(({ key, title }) =>
    Math.max(title.length, ...data.map((datum) => datum[key].length))
  );
  [
    headers.map(({ title }) => title.toUpperCase()),
    widths.map((width) => "-".repeat(width)),
    ...data.map((datum) => headers.map(({ key }) => datum[key])),
  ].map((values) =>
    console.log(
      values.map((value, idx) => value.padEnd(widths[idx])).join("  ")
    )
  );
}

// Parse command-line arguments, run main functionality, and exit.
async function main() {
  const program = new Command();
  program.option("--execute", "pull and build artifacts locally");
  program.option(
    "--publish",
    "with --execute, also deploy newly built artifacts"
  );
  program.option("--show-all", "show also unchanged artifacts");
  program.option(
    "--omit-unneeded-downloads",
    "don't download artifacts unless needed for dependent builds"
  );
  program.parse(process.argv);
  let plan = await computePlan();
  let tableData = plan.map(
    ({
      id,
      deps,
      artifact,
      name,
      desired,
      local,
      remote,
      download,
      build,
      upload,
    }) => {
      let action, details, func, couldPrune, noop;
      if (remote === desired && local === desired) {
        action = "(no action)";
        details = desired;
        func = () => {};
        couldPrune = true;
        noop = true;
      } else if (remote === desired && local !== desired) {
        action = "download remote";
        details = `${local} => ${desired}`;
        func = download;
        couldPrune = true;
        noop = false;
      } else if (local === desired && remote !== desired) {
        action = "publish local";
        details = `${remote} => ${desired}`;
        func = async () => {
          if (program.publish) {
            await upload();
          }
        };
        couldPrune = false;
        noop = false;
      } else {
        action = "rebuild and publish";
        if (local === remote) {
          details = `${local} => ${desired}`;
        } else {
          details = `${local} (local), ${remote} (remote) => ${desired}`;
        }
        func = async () => {
          await build();
          if (program.publish) {
            await upload();
          }
        };
        couldPrune = false;
        noop = false;
      }
      return {
        id,
        deps,
        couldPrune,
        artifact,
        name,
        action,
        details,
        func,
        noop,
      };
    }
  );
  if (program.omitUnneededDownloads) {
    for (const datum of [...tableData].reverse()) {
      if (
        datum.couldPrune &&
        _.every(
          tableData,
          (otherDatum) =>
            otherDatum.couldPrune || !otherDatum.deps.includes(datum.id)
        )
      ) {
        datum.pruned = true;
        if (!datum.noop) {
          datum.action += " [skipping]";
          datum.noop = true;
        }
      }
    }
  }
  console.log();
  const filteredTableData = tableData.filter(({ noop }) => !noop);
  if (filteredTableData.length === 0) {
    console.log(`*** NO ACTION REQUIRED FOR ${plan.length} ARTIFACTS ***`);
  } else {
    console.log(
      `*** ACTION REQUIRED FOR ${filteredTableData.length} of ${plan.length} ARTIFACTS ***`
    );
  }
  console.log();
  if (!program.showAll) {
    tableData = filteredTableData;
  }
  if (tableData.length > 0) {
    printTable(tableData, [
      { key: "artifact", title: "Type" },
      { key: "name", title: "Name" },
      { key: "action", title: "Action" },
      { key: "details", title: "Details" },
    ]);
    console.log();
  }
  tableData = filteredTableData;
  if (program.execute) {
    for (const { func } of tableData) {
      await func();
    }
  }
  process.exit(0);
}

if (process.argv[1] === url.fileURLToPath(import.meta.url)) {
  main().catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
