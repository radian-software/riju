import crypto from "crypto";
import child_process from "child_process";
import { promises as fs } from "fs";
import path from "path";
import util from "util";

import { parse } from "@babel/parser";
import { simple as babelWalk } from "babel-walk";

import { readLangConfig } from "./yaml.js";

async function getRelativeImports(filename) {
  const relativeImports = [];
  const program = parse(await fs.readFile(filename, "utf-8"), {
    sourceType: "module",
    plugins: ["classProperties"],
  });
  babelWalk({
    ImportDeclaration: (node) => {
      if (node.source.type !== "StringLiteral") {
        throw new Error(`unsupported import syntax:`, node);
      }
      const source = node.source.value;
      if (!source.startsWith(".")) {
        return;
      }
      relativeImports.push(source);
    },
  })(program);
  return relativeImports;
}

function pathRelativeTo(relativePath, relativeTo) {
  return path.join(path.dirname(path.resolve(relativeTo)), relativePath);
}

async function getTransitiveRelativeImports(filename) {
  let queue = [filename];
  const found = new Set();
  while (queue.length > 0) {
    const filename = path.resolve(queue.pop());
    if (found.has(filename)) {
      continue;
    }
    found.add(filename);
    queue = queue.concat(
      (await getRelativeImports(filename)).map((result) =>
        pathRelativeTo(result, filename)
      )
    );
  }
  return [...found];
}

async function getTestRunnerHash() {
  const files = await getTransitiveRelativeImports("backend/test-runner.js");
  files.push("package.json");
  files.push("yarn.lock");
  files.push("system/src/riju-system-privileged.c");
  const hashes = [];
  for (const file of files) {
    hashes.push(
      crypto
        .createHash("sha1")
        .update(await fs.readFile(file, "utf-8"))
        .digest("hex")
    );
  }
  return crypto.createHash("sha1").update(hashes.join(",")).digest("hex");
}

const testRunnerHash = getTestRunnerHash();

async function getTestConfigHash(lang) {
  const config = Object.assign({}, await readLangConfig(lang));
  delete config["install"];
  delete config["info"];
  return crypto.createHash("sha1").update(JSON.stringify(config)).digest("hex");
}

export async function getTestHash(lang, runtimeImageHash, langImageHash) {
  return crypto
    .createHash("sha1")
    .update(
      `${await testRunnerHash},${await getTestConfigHash(
        lang
      )},${runtimeImageHash},${langImageHash}`
    )
    .digest("hex");
}
