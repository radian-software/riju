import crypto from "crypto";
import fsBase, { promises as fs } from "fs";
import nodePath from "path";
import process from "process";
import url from "url";

import { Command } from "commander";
import dockerfileParser from "docker-file-parser";
import dockerignore from "@balena/dockerignore";
import _ from "lodash";

import { runCommand } from "./util.js";

// Given a string like "runtime" that identifies the relevant
// Dockerfile, read it from disk and parse it into a list of commands.
async function parseDockerfile(name) {
  const contents = await fs.readFile(`docker/${name}/Dockerfile`, "utf-8");
  return dockerfileParser.parse(contents);
}

// Read .dockerignore from disk and parse it into a dockerignore
// object that can be used to filter paths.
async function parseDockerignore() {
  const contents = await fs.readFile(".dockerignore", "utf-8");
  return dockerignore().add(contents.trim().split("\n"));
}

// Given a path (file or directory) relative to the repo root, read it
// and any children recursively. Return an array including the given
// path as well as any children, in lexicographic order by path
// segment. The array contains objects with the following properties:
//
// * path: string, always starts with passed-in path
// * type: string, 'file' or 'symlink' or 'directory'
// * executable: boolean (only for type: 'file')
// * hash: string, SHA-1 of contents (only for type: 'file')
// * target: string, target of symlink (only for type: 'symlink')
async function listFiles(path) {
  const stat = await fs.lstat(path);
  if (stat.isDirectory()) {
    const dirents = await fs.readdir(path, { withFileTypes: true });
    const subpaths = await Promise.all(
      _.sortBy(dirents, "name").map(async (dirent) => {
        return await listFiles(nodePath.join(path, dirent.name));
      })
    );
    return [{ path, type: "directory" }, ...subpaths.flat()];
  } else if (stat.isFile()) {
    const executable =
      (stat.mode &
        (fsBase.constants.S_IXUSR |
          fsBase.constants.S_IXGRP |
          fsBase.constants.S_IXOTH)) !=
      0;
    const contents = await fs.readFile(path, "utf-8");
    const hash = crypto.createHash("sha1").update(contents).digest("hex");
    return [{ path, type: "file", executable, hash }];
  } else if (stat.isSymbolicLink()) {
    const target = await fs.readlink(path);
    return [{ path, type: "symlink", target }];
  } else {
    return [];
  }
}

// Given a Dockerfile name like "packaging", read all the necessary
// files from disk and then convert the Dockerfile into a JavaScript
// object which includes all relevant build context. The idea is that
// whenever the Dockerfile would need to be rebuilt, something would
// change in this object, and when irrelevant things change, this
// object does not change.
//
// Options:
//
// * remote: fetch Riju image digests from registry instead of local
//     index
async function encodeDockerfile(name, opts) {
  const { remote } = opts || {};
  const dockerfile = await parseDockerfile(name);
  const ignore = await parseDockerignore();
  return await Promise.all(
    dockerfile.map(async ({ name, args, error }) => {
      if (error) {
        throw error;
      }
      const step = { name, args };
      outOfSwitch: switch (name) {
        case "ADD":
          throw new Error(
            `in Dockerfile ${name}, unsupported ADD directive used`
          );
        case "COPY":
          let nonOptionArgs = args;
          while (
            nonOptionArgs.length > 0 &&
            nonOptionArgs[0].startsWith("--")
          ) {
            if (nonOptionArgs[0].startsWith("--from")) {
              break outOfSwitch;
            }
            nonOptionArgs = nonOptionArgs.slice(1);
          }
          const sources = args.slice(0, nonOptionArgs.length - 1);
          const target = args[nonOptionArgs.length - 1];
          for (const source of sources) {
            for (const char of "*?[^]\\") {
              if (source.includes(char)) {
                throw new Error(
                  `in Dockerfile ${name}, COPY source ${source} uses unsupported wildcard`
                );
              }
            }
          }
          step.context = await Promise.all(
            sources.map(async (source) =>
              (await listFiles(source)).filter(
                (entry) => !ignore.ignores(entry.path)
              )
            )
          );
          break;
        case "FROM":
          if (typeof args !== "string") {
            throw new Error("got unexpected non-string for FROM args");
          }
          let image = args.split(" ")[0];
          let [repo, tag] = image.split(":");
          if (repo === "riju" && remote) {
            repo = process.env.DOCKER_REPO;
            if (!repo) {
              throw new Error("$DOCKER_REPO not set");
            }
          }
          image = `${repo}:${tag}`;
          if (remote) {
            const tags = (
              await runCommand(
                `skopeo list-tags "docker://${repo}" | jq -r '.Tags[]'`,
                { getStdout: true }
              )
            ).stdout
              .trim()
              .split("\n");
            if (tags.includes(tag)) {
              step.digest = (
                await runCommand(
                  `skopeo inspect docker://${image} | jq -r .Digest`,
                  { getStdout: true }
                )
              ).stdout.trim();
            } else {
              step.digest = "none";
            }
          } else {
            step.digest =
              (
                await runCommand(
                  `docker images --no-trunc --quiet "${image}"`,
                  { getStdout: true }
                )
              ).stdout.trim() || "none";
          }
          break;
      }
      return step;
    })
  );
}

// Parse command-line arguments, run main functionality, and exit.
async function main() {
  const program = new Command();
  program
    .arguments("<name>")
    .storeOptionsAsProperties(false)
    .option("--debug", "output Dockerfile internal representation, unhashed")
    .option("--remote", "fetch image digests from remote registry");
  program.parse(process.argv);
  if (program.args.length !== 1) {
    program.help();
  }
  const [name] = program.args;
  const { debug, remote } = program.opts();
  const encoding = await encodeDockerfile(name, { remote });
  if (debug) {
    console.log(JSON.stringify(encoding, null, 2));
  } else {
    const hash = crypto
      .createHash("sha1")
      .update(JSON.stringify(encoding))
      .digest("hex");
    console.log(hash);
  }
  process.exit(0);
}

if (process.argv[1] === url.fileURLToPath(import.meta.url)) {
  main().catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
