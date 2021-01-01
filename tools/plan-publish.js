import crypto from "crypto";
import { promises as fs } from "fs";
import process from "process";
import url from "url";

import { Command } from "commander";

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
  const DOCKER_REPO = getDockerRepo();
  const desired = await hashDockerfile(name, dependentHashes, opts);
  const local = await getLocalImageLabel(`riju:${name}`, "riju.image-hash");
  const remote = await getRemoteImageLabel(
    `${DOCKER_REPO}:${name}`,
    "riju.image-hash"
  );
  dependentHashes[`${DOCKER_REPO}:${name}`] = desired;
  return {
    artifact: "Docker image",
    name,
    desired,
    local,
    remote,
    download: async () => {
      await runCommand(`make pull I=${name}`);
    },
    build: async () => {
      await runCommand(`make image I=${name}`);
    },
    upload: async () => {
      await runCommand(`make push I=${name}`);
    },
  };
}

async function planDebianPackages() {
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
  return await Promise.all(
    (await getPackages()).map(
      async ({ lang, type, name, buildScriptPath, debPath }) => {
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
              await runCommand(`dpkg-deb -f ${debPath} Riju-Script-Hash`)
            ).stdout.trim() || null;
        }
        const remote = remoteHashes[name] || null;
        return {
          artifact: "Debian package",
          name,
          desired,
          local,
          remote,
          download: async () => {
            await runCommand(`make download L=${lang} T=${type}`);
          },
          build: async () => {
            await runCommand(`make pkg L=${lang} T=${type}`);
          },
          upload: async () => {
            await runCommand(`make upload L=${lang} T=${type}`);
          },
        };
      }
    )
  );
}

async function computePlan() {
  const dependentHashes = {};
  const packaging = await planDockerImage("packaging", dependentHashes);
  const runtime = await planDockerImage("runtime", dependentHashes);
  const packages = await planDebianPackages();
  const packageHashes = packages.map(({ desired }) => desired).sort();
  const composite = await planDockerImage("composite", dependentHashes, {
    salt: { packageHashes },
  });
  const compile = await planDockerImage("compile", dependentHashes);
  const app = await planDockerImage("app", dependentHashes);
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
  program.option("--publish", "deploy newly built artifacts");
  program.option("--all", "show also unchanged artifacts");
  program.parse(process.argv);
  const plan = await computePlan();
  const filteredPlan = plan.filter(({ desired, remote }) => desired !== remote);
  console.log();
  if (filteredPlan.length === 0) {
    console.log(`*** NO CHANGES REQUIRED TO ${plan.length} ARTIFACTS ***`);
    if (!program.all) {
      plan = filteredPlan;
    }
  } else {
    console.log(
      `*** CHANGES REQUIRED TO ${filteredPlan.length} of ${plan.length} ARTIFACTS ***`
    );
  }
  console.log();
  if (plan.length === 0) {
    process.exit(0);
  }
  const tableData = plan.map(
    ({ artifact, name, desired, local, remote, download, build, upload }) => {
      let action, details, func;
      if (remote === desired && local === desired) {
        action = "(no action)";
        details = desired;
        func = () => {};
      } else if (remote === desired && local !== desired) {
        action = "download remote";
        details = `${local} (local) => ${desired}`;
        func = download;
      } else if (local === desired && remote !== desired) {
        action = "publish local";
        details = `${remote} (remote) => ${desired}`;
        func = upload;
      } else {
        action = "rebuild and publish";
        if (local === remote) {
          details = `${local} (local) => ${desired}`;
        } else {
          details = `${local} (local), ${remote} (remote) => ${desired}`;
        }
        func = async () => {
          await build();
          await upload();
        };
      }
      return { artifact, name, action, details, func };
    }
  );
  printTable(tableData, [
    { key: "artifact", title: "Type" },
    { key: "name", title: "Name" },
    { key: "action", title: "Action" },
    { key: "details", title: "Details" },
  ]);
  console.log();
  if (program.publish) {
    for ({ func } of tableData) {
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
