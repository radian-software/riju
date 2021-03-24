import crypto from "crypto";
import { promises as fs } from "fs";
import process from "process";
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
} from "./docker-util.js";
import { getBaseImages, hashDockerfile } from "./hash-dockerfile.js";
import { runCommand } from "./util.js";

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
              `aws s3api list-objects-v2 --bucket riju-debs --prefix hashes`,
              { getStdout: true }
            )
          ).stdout
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
              `aws s3api list-objects-v2 --bucket riju-debs --prefix test-hashes/lang`,
              { getStdout: true }
            )
          ).stdout
        ).Contents.map(({ Key: key }) => {
          const [_1, _2, remoteName, remoteHash] = key.split("/");
          return [remoteName, remoteHash];
        })
      );
    },
  };
}

async function getImageArtifact({ tag, isBaseImage, isLangImage }) {
  const DOCKER_REPO = getDockerRepo();
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
    getLocalHash: async () => {
      return await getLocalImageLabel(`riju:${tag}`, "riju.image-hash");
    },
    getPublishedHash: async () => {
      return await getRemoteImageLabel(
        `${DOCKER_REPO}:${tag}`,
        "riju.image-hash"
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
        salt = {
          langHash: dependencyHashes[`deb:lang-${isLangImage.lang}`],
          sharedHashes: isLangImage.sharedDeps.map(
            (name) => dependencyHashes[`deb:shared-${name}`]
          ),
        };
      }
      return await hashDockerfile(name, dependentDockerHashes, { salt });
    },
    buildLocally: async () => {
      await runCommand(`make image I=${tag}`);
    },
    retrieveFromRegistry: async () => {
      await runCommand(`make pull I=${tag}`);
    },
    publishToRegistry: async () => {
      await runCommand(`make push I=${tag}`);
    },
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
      return s3TestHashes[lang];
    },
    getDesiredHash: async (dependencyHashes) => {
      return await getTestHash(lang, dependencyHashes["image:runtime"]);
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
        `aws s3 cp ${hashPath} s3://${S3_BUCKET}/test-hashes/lang/${lang}/${hash}`
      );
    },
  };
}

async function getDeployArtifact(langs) {
  return {
    name: `deploy:prod`,
    dependencies: ["image:app"]
      .concat(langs.map((lang) => `image:lang-${lang}`))
      .concat(langs.map((lang) => `test:lang-${lang}`)),
    publishOnly: true,
    publishToRegistry: async () => {
      await runCommand(`tools/deploy.bash`);
    },
  };
}

async function getDepGraph() {
  const informationalDependencies = getInformationalDependencies();
  const artifacts = [];
  artifacts.push(
    await getImageArtifact({
      tag: "ubuntu",
      isBaseImage: true,
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
  artifacts.push(await getDeployArtifact(langs));
  return { informationalDependencies, artifacts };
}

function getTransitiveDependencies({ artifacts, targets }) {
  let queue = targets;
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

async function executeDepGraph({ depgraph, manual, publish, yes, targets }) {
  const artifacts = {};
  for (const artifact of depgraph.artifacts) {
    artifacts[artifact.name] = artifact;
  }
  if (manual) {
    for (const target of targets) {
      if (artifacts[target].dependencies.length > 0) {
        throw new Error(
          `cannot build target ${target} with --manual as it is not a leaf artifact`
        );
      }
    }
  }
  const transitiveTargets = getTransitiveDependencies({ artifacts, targets });
  const requiredInfo = new Set();
  for (const target of transitiveTargets) {
    for (const name of Object.values(
      artifacts[target].informationalDependencies || {}
    )) {
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
    promises.local[target] = artifacts[target].getLocalHash(info);
    promises.published[target] = artifacts[target].getPublishedHash(info);
    promises.desired[target] = (async () => {
      if (manual) {
        // Artifact has no dependencies in this case.
        return await artifacts[target].getDesiredHash({});
      }
      const dependencyHashes = {};
      for (const dependency of artifacts[target].dependencies) {
        dependencyHashes[dependency] = await promises.desired[dependency];
      }
      let hash = await artifacts[target].getDesiredHash(dependencyHashes);
      if (hash !== null) {
        return hash;
      }
      // If hash is missing, cast about in blind panic for another
      // possible way to compute it.
      hash = await promises.published[target];
      if (hash !== null) {
        return hash;
      }
      hash = await promises.local[target];
      if (hash !== null) {
        return hash;
      }
      throw new Error(
        `artifact must be built manually: dep ${target} --manual [--publish]`
      );
    })();
  }
  await Promise.all(
    transitiveTargets.map(async (target) => {
      const {
        publishOnly,
        getLocalHash,
        getPublishedHash,
        getDesiredHash,
      } = artifacts[target];
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
  console.log(hashes);
}

async function main() {
  const program = new Command();
  program.usage("<target>...");
  program.option("--list", "list available artifacts; ignore other arguments");
  program.option("--manual", "operate manually on leaf artifacts");
  program.option("--publish", "publish artifacts to remote registries");
  program.option("--yes", "execute plan without confirmation");
  program.parse(process.argv);
  const { list, manual, publish, yes } = program.opts();
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
  await executeDepGraph({
    depgraph,
    manual,
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
