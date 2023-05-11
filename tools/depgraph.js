import crypto from "crypto";
import { promises as fs } from "fs";
import process from "process";
import readline from "readline";
import url from "url";

import { Command } from "commander";
import _ from "lodash";

import { getTestHash } from "../lib/hash-test.js";
import {
  getLangs,
  getSharedDeps,
  getSharedDepsForLangConfig,
  readLangConfig,
} from "../lib/yaml.js";
import {
  getDockerRepo,
  getLocalImageLabel,
  getRemoteImageLabel,
  getRemoteRepositoryTags,
} from "./docker-util.js";
import { getBaseImages, hashDockerfile } from "./hash-dockerfile.js";
import { runCommand } from "./util.js";

const CONCURRENCY = 1;

async function allPromises(callables, { concurrency }) {
  const queue = new PQueue({ concurrency: concurrency });
  const results = [];
  for (const callable of callables) {
    queue.add(async () => {
      console.log("START");
      results.push(await callable());
      console.log("END");
    });
  }
  await queue.onIdle();
  return results;
}

function getS3Bucket() {
  if (!process.env.S3_BUCKET) {
    throw new Error(`unset environment variable: \$S3_BUCKET`);
  }
  return process.env.S3_BUCKET;
}

function getInformationalDependencies() {
  return {
    s3DebHashes: async () => {
      return Object.fromEntries(
        JSON.parse(
          (
            await runCommand(
              `aws s3api list-objects-v2 --bucket '${getS3Bucket()}' --prefix hashes`,
              { getStdout: true }
            )
          ).stdout || '{"Contents": []}'
        ).Contents.map(({ Key: key }) => {
          const [_, remoteName, remoteHash] = key.split("/");
          return [remoteName, remoteHash];
        })
      );
    },
    s3TestHashes: async () => {
      return Object.fromEntries(
        JSON.parse(
          (
            await runCommand(
              `aws s3api list-objects-v2 --bucket '${getS3Bucket()}' --prefix test-hashes/lang`,
              { getStdout: true }
            )
          ).stdout || '{"Contents": []}'
        ).Contents.map(({ Key: key }) => {
          const [_1, _2, remoteName, remoteHash] = key.split("/");
          return [remoteName, remoteHash];
        })
      );
    },
    dockerRepoTags: async () => {
      return await getRemoteRepositoryTags(getDockerRepo());
    },
  };
}

async function getImageArtifact({ tag, isBaseImage, isLangImage }) {
  const name = isLangImage ? "lang" : tag;
  let baseImageTags = [];
  let dependencies = [];
  if (!isBaseImage) {
    baseImageTags = [...new Set(await getBaseImages(name))].map((baseImage) => {
      if (!baseImage.startsWith("riju:")) {
        throw new Error(
          `non-Riju base image '${baseImage}' in Dockerfile for ${name} image`
        );
      }
      return baseImage.replace(/^riju:/, "");
    });
    dependencies = baseImageTags.map((baseImageTag) => `image:${baseImageTag}`);
  }
  if (isLangImage) {
    dependencies.push(`deb:lang-${isLangImage.lang}`);
    dependencies = dependencies.concat(
      isLangImage.sharedDeps.map((name) => `deb:shared-${name}`)
    );
  }
  return {
    name: `image:${tag}`,
    dependencies: dependencies,
    informationalDependencies: {
      getPublishedHash: "dockerRepoTags",
    },
    getLocalHash: async () => {
      return await getLocalImageLabel(`riju:${tag}`, "riju.image-hash");
    },
    getPublishedHash: async ({ dockerRepoTags }) => {
      const DOCKER_REPO = getDockerRepo();
      if (!dockerRepoTags.includes(tag)) {
        return null;
      }
      return await getRemoteImageLabel(
        `${DOCKER_REPO}:${tag}`,
        "riju.image-hash",
        dockerRepoTags
      );
    },
    getDesiredHash: async (dependencyHashes) => {
      if (isBaseImage) {
        return null;
      }
      const dependentDockerHashes = {};
      for (const baseImageTag of baseImageTags) {
        dependentDockerHashes[`riju:${baseImageTag}`] =
          dependencyHashes[`image:${baseImageTag}`];
      }
      let salt = null;
      if (isLangImage) {
        const installContents = await fs.readFile(
          `build/lang/${isLangImage.lang}/install.bash`,
          "utf-8"
        );
        const sharedInstallContents = await Promise.all(
          isLangImage.sharedDeps.map(async (name) =>
            fs.readFile(`build/shared/${name}/install.bash`)
          )
        );
        const allInstallContents = [].concat.apply(
          [installContents],
          sharedInstallContents
        );
        salt = {
          langHash: dependencyHashes[`deb:lang-${isLangImage.lang}`],
          sharedHashes: isLangImage.sharedDeps.map(
            (name) => dependencyHashes[`deb:shared-${name}`]
          ),
          installHash: allInstallContents
            .map((c) => crypto.createHash("sha1").update(c).digest("hex"))
            .join(""),
        };
      }
      return await hashDockerfile(name, dependentDockerHashes, { salt });
    },
    buildLocally: async () => {
      if (isLangImage) {
        await runCommand(`make image I=${name} L=${isLangImage.lang}`);
      } else {
        await runCommand(`make image I=${name}`);
      }
    },
    retrieveFromRegistry: async () => {
      await runCommand(`make pull I=${tag}`);
    },
    publishToRegistry: async () => {
      try {
        await runCommand(`make push I=${tag}`);
      } catch (e) {
        console.log(e);
      }
    },
    syncCommand: isBaseImage,
  };
}

async function getDebArtifact({ type, lang }) {
  return {
    name: `deb:${type}-${lang}`,
    dependencies: ["image:packaging"],
    informationalDependencies: {
      getPublishedHash: "s3DebHashes",
    },
    getLocalHash: async () => {
      const debPath = `build/${type}/${lang}/riju-${type}-${lang}.deb`;
      try {
        await fs.access(debPath);
      } catch (err) {
        return null;
      }
      return (
        (
          await runCommand(`dpkg-deb -f ${debPath} Riju-Script-Hash`, {
            getStdout: true,
          })
        ).stdout.trim() || null
      );
    },
    getPublishedHash: async ({ s3DebHashes }) => {
      return s3DebHashes[`riju-${type}-${lang}`] || null;
    },
    getDesiredHash: async (dependencyHashes) => {
      let contents = await fs.readFile(
        `build/${type}/${lang}/build.bash`,
        "utf-8"
      );
      contents += dependencyHashes["image:packaging"] + "\n";
      return crypto.createHash("sha1").update(contents).digest("hex");
    },
    buildLocally: async () => {
      await runCommand(
        `make shell I=packaging CMD="make pkg T=${type} L=${lang}"`
      );
    },
    retrieveFromRegistry: async () => {
      await runCommand(`make download T=${type} L=${lang}`);
    },
    publishToRegistry: async () => {
      await runCommand(`make upload T=${type} L=${lang}`);
    },
  };
}

async function getLanguageTestArtifact({ lang }) {
  return {
    name: `test:lang-${lang}`,
    dependencies: ["image:runtime", `image:lang-${lang}`],
    informationalDependencies: {
      getPublishedHash: "s3TestHashes",
      retrieveFromRegistry: "s3TestHashes",
    },
    getLocalHash: async () => {
      const hashPath = `build/test-hashes/lang/${lang}`;
      let hash;
      try {
        return (await fs.readFile(hashPath, "utf-8")).trim();
      } catch (err) {
        if (err.code === "ENOENT") {
          return null;
        } else {
          throw err;
        }
      }
    },
    getPublishedHash: async ({ s3TestHashes }) => {
      return s3TestHashes[lang] || null;
    },
    getDesiredHash: async (dependencyHashes) => {
      return await getTestHash(
        lang,
        dependencyHashes[`image:runtime`],
        dependencyHashes[`image:lang-${lang}`]
      );
    },
    buildLocally: async () => {
      await runCommand(`make shell I=runtime CMD="make test L=${lang}"`);
    },
    retrieveFromRegistry: async ({ s3TestHashes }) => {
      await fs.writeFile(
        `build/test-hashes/lang/${lang}`,
        s3TestHashes[lang] + "\n"
      );
    },
    publishToRegistry: async () => {
      const hashPath = `build/test-hashes/lang/${lang}`;
      const hash = (await fs.readFile(hashPath, "utf-8")).trim();
      const S3_BUCKET = getS3Bucket();
      await runCommand(
        `aws s3 rm --recursive s3://${S3_BUCKET}/test-hashes/lang/${lang}`
      );
      await runCommand(
        `aws s3 cp ${hashPath} s3://${S3_BUCKET}/test-hashes/lang/${lang}/${hash}`
      );
    },
  };
}

async function getDeployReadyArtifact(langs) {
  return {
    name: `deploy:ready`,
    dependencies: ["image:app"]
      .concat(langs.map((lang) => `image:lang-${lang}`))
      .concat(langs.map((lang) => `test:lang-${lang}`)),
    publishTarget: true,
    publishToRegistry: async () => {
      await runCommand(`make deploy-config`);
    },
  };
}

async function getDeployLiveArtifact(langs) {
  return {
    name: `deploy:live`,
    dependencies: ["deploy:ready"],
    publishTarget: true,
    publishToRegistry: async () => {
      await runCommand(`make deploy-latest`);
    },
  };
}

async function getDepGraph() {
  const informationalDependencies = getInformationalDependencies();
  const artifacts = [];
  artifacts.push(
    await getImageArtifact({
      tag: "ubuntu",
      isBaseImage: "make sync-ubuntu",
    })
  );
  artifacts.push(await getImageArtifact({ tag: "packaging" }));
  artifacts.push(await getImageArtifact({ tag: "base" }));
  for (const sharedDep of await getSharedDeps()) {
    artifacts.push(await getDebArtifact({ type: "shared", lang: sharedDep }));
  }
  const langs = await getLangs();
  const langConfigs = Object.fromEntries(
    await Promise.all(
      langs.map(async (lang) => [lang, await readLangConfig(lang)])
    )
  );
  artifacts.push(await getImageArtifact({ tag: "runtime" }));
  for (const lang of langs) {
    artifacts.push(await getDebArtifact({ type: "lang", lang: lang }));
    artifacts.push(
      await getImageArtifact({
        tag: `lang-${lang}`,
        isLangImage: {
          lang: lang,
          sharedDeps: await getSharedDepsForLangConfig(langConfigs[lang]),
        },
      })
    );
    artifacts.push(await getLanguageTestArtifact({ lang: lang }));
  }
  artifacts.push(await getImageArtifact({ tag: "app" }));
  artifacts.push(await getDeployReadyArtifact(langs));
  artifacts.push(await getDeployLiveArtifact(langs));
  return { informationalDependencies, artifacts };
}

function getTransitiveDependencies({ artifacts, targets }) {
  let queue = [...targets];
  let found = new Set();
  while (queue.length > 0) {
    const name = queue.pop();
    if (found.has(name)) {
      continue;
    }
    if (!artifacts[name]) {
      throw new Error(`no such artifact: ${name}`);
    }
    queue = queue.concat(artifacts[name].dependencies);
    found.add(name);
  }
  return _.sortBy([...found], (name) => Object.keys(artifacts).indexOf(name));
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

async function getUserInput(prompt) {
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    prompt: prompt,
  });
  rl.prompt();
  let resolved = false;
  const resp = await new Promise((resolve, reject) => {
    rl.on("line", (line) => {
      resolved = true;
      resolve(line);
    });
    rl.on("close", () => {
      if (!resolved) {
        console.log();
        console.log();
        reject(new Error("cancelled"));
      }
    });
  });
  rl.close();
  return resp;
}

async function executeDepGraph({
  depgraph,
  manual,
  holdManual,
  all,
  registry,
  publish,
  yes,
  targets,
}) {
  const artifacts = {};
  for (const artifact of depgraph.artifacts) {
    for (const dep of artifact.dependencies) {
      if (!artifacts[dep]) {
        throw new Error(
          `artifact ${artifact.name} appears before dependency ${dep} in depgraph`
        );
      }
    }
    artifacts[artifact.name] = artifact;
  }
  const transitiveTargets = getTransitiveDependencies({ artifacts, targets });
  const requiredInfo = new Set();
  for (const target of transitiveTargets) {
    for (const [method, name] of Object.entries(
      artifacts[target].informationalDependencies || {}
    )) {
      if (
        !(
          !registry &&
          ["getPublishedHash", "retrieveFromRegistry"].includes(method)
        )
      )
        requiredInfo.add(name);
    }
  }
  const info = {};
  await Promise.all(
    [...requiredInfo].map(async (name) => {
      info[name] = await depgraph.informationalDependencies[name]();
    })
  );
  const hashes = {
    local: {},
    published: {},
    desired: {},
  };
  const promises = {
    local: {},
    published: {},
    desired: {},
  };
  for (const target of transitiveTargets) {
    if (artifacts[target].publishTarget) {
      promises.local[target] = Promise.resolve(null);
      promises.published[target] = Promise.resolve(null);
      promises.desired[target] = Promise.resolve(null);
    } else {
      promises.local[target] = artifacts[target].getLocalHash(info);
      promises.published[target] = registry
        ? artifacts[target].getPublishedHash(info)
        : Promise.resolve(null);
      promises.desired[target] = (async () => {
        const dependencyHashes = {};
        for (const dependency of artifacts[target].dependencies) {
          dependencyHashes[dependency] = await promises.desired[dependency];
          if (!dependencyHashes[dependency]) {
            throw new Error(
              `manual artifact must be retrieved explicitly: run '${artifacts[dependency].syncCommand}' (or dep '${dependency} --manual' to update)`
            );
          }
        }
        let hash = await artifacts[target].getDesiredHash(dependencyHashes);
        if (hash || manual) {
          return hash;
        }
        const promiseSets = [promises.published, promises.local];
        if (holdManual) {
          promiseSets.reverse();
        }
        for (const promiseSet of promiseSets) {
          const hash = await promiseSet[target];
          if (hash) {
            return hash;
          }
        }
        throw new Error(
          `manual artifact must be retrieved explicitly: run '${artifacts[target].syncCommand}' (or dep '${target} --manual' to update)`
        );
      })();
    }
  }
  await Promise.all(
    transitiveTargets.map(async (target) => {
      await Promise.all([
        promises.local[target].then((hash) => {
          hashes.local[target] = hash;
        }),
        promises.published[target].then((hash) => {
          hashes.published[target] = hash;
        }),
        promises.desired[target].then((hash) => {
          hashes.desired[target] = hash;
        }),
      ]);
    })
  );
  const statuses = {};

  for (const name in artifacts) {
    if (!hashes.desired[name]) {
      statuses[name] = "buildLocally";
    } else if (
      hashes.published[name] === hashes.desired[name] &&
      hashes.local[name] === hashes.desired[name]
    ) {
      statuses[name] = null;
    } else if (
      hashes.local[name] === hashes.desired[name] &&
      hashes.published[name] !== hashes.desired[name]
    ) {
      statuses[name] = "publishToRegistry";
    } else if (
      hashes.published[name] === hashes.desired[name] &&
      hashes.local[name] !== hashes.desired[name]
    ) {
      statuses[name] = "retrieveFromRegistry";
    } else {
      statuses[name] = "buildLocally";
    }
  }
  let priorityTargets;
  if (all) {
    priorityTargets = transitiveTargets;
  } else {
    const queue = [...targets];
    const found = new Set();
    while (queue.length > 0) {
      const name = queue.pop();
      if (found.has(name)) {
        continue;
      }
      for (const dep of artifacts[name].dependencies) {
        if (statuses[dep] === "buildLocally") {
          queue.push(dep);
        }
      }
      found.add(name);
    }
    priorityTargets = [...found];
  }
  priorityTargets = _.sortBy(priorityTargets, (name) =>
    Object.keys(artifacts).indexOf(name)
  );
  const plan = [];
  const seen = new Set();
  for (const target of priorityTargets) {
    for (const dep of artifacts[target].dependencies) {
      if (seen.has(dep)) {
        continue;
      }
      if (artifacts[target].publishTarget) {
        if (statuses[dep] === "publishToRegistry" && publish) {
          plan.push({
            artifact: dep,
            action: "publishToRegistry",
          });
        }
      } else {
        if (statuses[dep] === "retrieveFromRegistry") {
          plan.push({
            artifact: dep,
            action: "retrieveFromRegistry",
          });
        }
      }
      seen.add(dep);
    }
    if (statuses[target]) {
      if (
        !artifacts[target].publishTarget &&
        !(statuses[target] === "publishToRegistry" && !publish)
      ) {
        plan.push({
          artifact: target,
          action: statuses[target],
        });
      }
      if (statuses[target] === "buildLocally" && publish) {
        plan.push({
          artifact: target,
          action: "publishToRegistry",
        });
      }
    }
    seen.add(target);
  }
  const shortnames = {
    buildLocally: "rebuild",
    retrieveFromRegistry: "download",
    publishToRegistry: "publish",
  };
  const totals = {};
  for (let { action } of plan) {
    action = shortnames[action];
    totals[action] = (totals[action] || 0) + 1;
  }
  console.log();
  if (plan.length === 0) {
    console.log("No updates needed.");
    return;
  }
  printTable(
    plan.map(({ artifact, action }) => {
      const desc = {
        buildLocally: "          rebuild",
        retrieveFromRegistry: "download",
        publishToRegistry: "                   publish",
      }[action];
      if (!desc) {
        throw new Error(`unexpected action key ${action}`);
      }
      return { artifact, desc };
    }),
    [
      { key: "artifact", title: "Artifact" },
      { key: "desc", title: "Action" },
    ]
  );
  console.log();
  console.log(
    `Plan: ` +
      ["download", "rebuild", "publish"]
        .filter((action) => totals[action])
        .map((action) => `${totals[action]} to ${action}`)
        .join(", ")
  );
  console.log();
  if (yes) {
    console.log("Skipping confirmation since --yes was passed.");
  } else {
    console.log("Do you want to perform these actions?");
    console.log("  Depgraph will perform the actions described above.");
    console.log("  Only 'yes' will be accepted to approve.");
    console.log();
    const resp = await getUserInput("  Enter a value: ");
    if (resp !== "yes") {
      console.log();
      console.log("Apply cancelled.");
      process.exit(1);
    }
  }
  for (let idx = 0; idx < plan.length; idx++) {
    const { artifact, action } = plan[idx];
    console.log();
    console.log(
      `===== Step ${idx + 1} of ${plan.length}: ${
        shortnames[action]
      } ${artifact} =====`
    );
    console.log();
    await artifacts[artifact][action](info);
  }
  console.log();
  console.log("Apply successful!");
}

async function main() {
  const program = new Command("dep");
  program.usage("<target>...");
  program.option("--list", "list available artifacts; ignore other arguments");
  program.option("--manual", "operate explicitly on manual artifacts");
  program.option("--hold-manual", "prefer local versions of manual artifacts");
  program.option("--all", "do not skip unneeded intermediate artifacts");
  program.option("--registry", "interface with remote registry for caching");
  program.option("--publish", "publish artifacts to remote registries");
  program.option("--yes", "execute plan without confirmation");
  program.parse(process.argv);
  const { list, manual, holdManual, all, registry, publish, yes } =
    program.opts();
  const depgraph = await getDepGraph();
  if (list) {
    for (const { name } of depgraph.artifacts) {
      console.log(name);
    }
    console.error();
    console.error(`${depgraph.artifacts.length} artifacts`);
    process.exit(0);
  }
  if (program.args.length === 0) {
    program.help({ error: true });
  }
  await runCommand("make all-scripts system");
  await executeDepGraph({
    depgraph,
    manual,
    holdManual,
    all,
    registry,
    publish,
    yes,
    targets: program.args,
  });
}

if (process.argv[1] === url.fileURLToPath(import.meta.url)) {
  main().catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
