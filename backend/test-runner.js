import { promises as fs } from "fs";
import process from "process";

import _ from "lodash";
import pQueue from "p-queue";
const PQueue = pQueue.default;
import stripAnsi from "strip-ansi";

import { getTestHash } from "../lib/hash-test.js";
import * as api from "./api.js";
import { langsPromise } from "./langs.js";
import { getUUID } from "./util.js";

let langs = {};

function parseIntOr(thing, def) {
  const num = parseInt(thing);
  return Number.isNaN(num) ? def : num;
}

const TIMEOUT = parseIntOr(process.env.TEST_TIMEOUT_SECS, 15);
const PATIENCE = parseIntOr(process.env.TEST_PATIENCE, 1);
const CONCURRENCY = parseIntOr(process.env.TEST_CONCURRENCY, 2);

function findPosition(str, idx) {
  const lines = str.substring(0, idx).split("\n");
  const line = lines.length - 1;
  const character = lines[lines.length - 1].length;
  return { line, character };
}

async function sendInput(send, input) {
  for (const line of input.split("\n")) {
    if (line === "EOF") {
      send({ event: "terminalInput", input: "\u0004" });
    } else if (line.startsWith("DELAY:")) {
      const delay = parseFloat(line.replace(/DELAY: */, ""));
      if (Number.isNaN(delay)) continue;
      await new Promise((resolve) =>
        setTimeout(resolve, delay * 1000 * PATIENCE)
      );
    } else {
      send({ event: "terminalInput", input: line + "\r" });
    }
  }
}

class Test {
  get config() {
    return langs[this.lang];
  }

  record = (msg) => {
    const dur = (new Date().getTime() - this.startTime) / 1000;
    this.messages.push({ time: dur, ...msg });
  };

  send = (msg) => {
    this.ws.onMessage(JSON.stringify(msg));
    this.record(msg);
    this.handledMessages += 1;
  };

  constructor(lang, type) {
    this.lang = lang;
    this.type = type;
    this.messages = [];
    this.timedOut = false;
    this.handledMessages = 0;
    this.handleUpdate = () => {};
    this.startTime = null;
    this.ws = null;
  }

  getLog = (opts) => {
    opts = opts || {};
    return this.messages
      .map((msg) => JSON.stringify(msg, null, opts.pretty && 2))
      .join("\n");
  };

  run = async () => {
    if ((this.config.skip || []).includes(this.type)) {
      return "skipped";
    }
    this.startTime = new Date().getTime();
    let session = null;
    let timeout = null;
    try {
      const that = this;
      this.ws = {
        on: function (type, handler) {
          switch (type) {
            case "message":
              this.onMessage = handler;
              for (const msg of this.messageQueue) {
                this.onMessage(msg);
              }
              this.messageQueue = [];
              break;
            case "close":
            case "error":
              // No need to clean up, we'll call teardown() explicitly.
              break;
            default:
              throw new Error(`unexpected websocket handler type: ${type}`);
          }
        },
        onMessage: function (msg) {
          this.messageQueue.push(msg);
        },
        messageQueue: [],
        send: function (data) {
          that.record(JSON.parse(data));
          that.handleUpdate();
        },
        terminate: function () {},
      };
      session = new api.Session(this.ws, this.lang, (msg) => {
        this.record({ event: "serverLog", message: msg });
      });
      timeout = setTimeout(() => {
        this.timedOut = true;
        this.handleUpdate();
      }, TIMEOUT * 1000 * PATIENCE);
      await session.setup();
      switch (this.type) {
        case "ensure":
          await this.testEnsure();
          break;
        case "run":
          await this.testRun();
          break;
        case "repl":
          await this.testRepl();
          break;
        case "runrepl":
          await this.testRunRepl();
          break;
        case "scope":
          await this.testScope();
          break;
        case "format":
          await this.testFormat();
          break;
        case "lsp":
          await this.testLsp();
          break;
        default:
          throw new Error(`Unexpected test type: ${this.type}`);
      }
    } finally {
      this.ws = null;
      if (timeout) {
        clearTimeout(timeout);
      }
      if (session) {
        await session.teardown();
      }
    }
  };

  wait = async (desc, handler) => {
    return await new Promise((resolve, reject) => {
      this.handleUpdate = () => {
        if (this.timedOut) {
          reject(new Error(`timeout while waiting for ${desc}`));
        } else {
          while (this.handledMessages < this.messages.length) {
            const msg = this.messages[this.handledMessages];
            const result = handler(msg);
            if (![undefined, null, false].includes(result)) {
              resolve(result);
            }
            this.handledMessages += 1;
          }
        }
      };
      this.handleUpdate();
    });
  };

  waitForOutput = async (pattern, maxLength) => {
    pattern = pattern.replace(/\n/g, "\r\n");
    let output = "";
    return await this.wait(`output ${JSON.stringify(pattern)}`, (msg) => {
      const prevLength = output.length;
      if (msg.event === "terminalOutput") {
        // Applying stripAnsi here is wrong because escape sequences
        // could be split across multiple messages. Who cares?
        output += stripAnsi(msg.output);
      }
      if (typeof maxLength === "number") {
        return (
          output
            .substring(prevLength - maxLength)
            .match(new RegExp(pattern)) !== null
        );
      } else {
        return output.indexOf(pattern, prevLength - pattern.length) != -1;
      }
    });
  };

  testEnsure = async () => {
    this.send({ event: "ensure" });
    const code = await this.wait("ensure response", (msg) => {
      if (msg.event === "ensured") {
        return msg.code;
      }
    });
    if (code !== 0) {
      throw new Error(`ensure failed with code ${code}`);
    }
  };
  testRun = async () => {
    const pattern = this.config.hello || "Hello, world!";
    this.send({ event: "runCode", code: this.config.template + "\n" });
    if (this.config.helloInput !== undefined) {
      sendInput(this.send, this.config.helloInput);
    }
    await this.waitForOutput(pattern, this.config.helloMaxLength);
  };
  testRepl = async () => {
    const input = this.config.input || "123 * 234";
    const output = this.config.output || "28782";
    sendInput(this.send, input);
    await this.waitForOutput(output);
  };
  testRunRepl = async () => {
    const input = this.config.runReplInput || this.config.input || "123 * 234";
    const output = this.config.runReplOutput || this.config.output || "28782";
    this.send({ event: "runCode", code: this.config.template + "\n" });
    sendInput(this.send, input);
    await this.waitForOutput(output);
  };
  testScope = async () => {
    const code = this.config.scope.code;
    const after = this.config.scope.after;
    const input = this.config.scope.input || "x";
    const output = this.config.scope.output || "28782";
    let allCode = this.config.template + "\n";
    if (after) {
      allCode = allCode.replace(after + "\n", after + "\n" + code + "\n");
    } else {
      allCode = allCode + code + "\n";
    }
    this.send({ event: "runCode", code: allCode });
    sendInput(this.send, input);
    await this.waitForOutput(output);
  };
  testFormat = async () => {
    const input = this.config.format.input + "\n";
    const output = (this.config.format.output || this.config.template) + "\n";
    this.send({ event: "formatCode", code: input });
    const result = await this.wait("formatter response", (msg) => {
      if (msg.event === "formattedCode") {
        return msg.code;
      }
    });
    if (output !== result) {
      throw new Error("formatted code did not match");
    }
  };
  testLsp = async () => {
    const template = this.config.template + "\n";
    const insertedCode = this.config.lsp.code;
    const after = this.config.lsp.after;
    const item = this.config.lsp.item;
    const idx = after
      ? template.indexOf(after) + after.length
      : template.length;
    const code = template.slice(0, idx) + insertedCode + template.slice(idx);
    const root = await this.wait("lspStarted message", (msg) => {
      if (msg.event === "lspStarted") {
        return msg.root;
      }
    });
    this.send({
      event: "lspInput",
      input: {
        jsonrpc: "2.0",
        id: "0d75333a-47d8-4da8-8030-c81d7bd9eed7",
        method: "initialize",
        params: {
          processId: null,
          clientInfo: { name: "vscode" },
          rootPath: root,
          rootUri: `file://${root}`,
          capabilities: {
            workspace: {
              applyEdit: true,
              workspaceEdit: {
                documentChanges: true,
                resourceOperations: ["create", "rename", "delete"],
                failureHandling: "textOnlyTransactional",
              },
              didChangeConfiguration: { dynamicRegistration: true },
              didChangeWatchedFiles: { dynamicRegistration: true },
              symbol: {
                dynamicRegistration: true,
                symbolKind: {
                  valueSet: [
                    1,
                    2,
                    3,
                    4,
                    5,
                    6,
                    7,
                    8,
                    9,
                    10,
                    11,
                    12,
                    13,
                    14,
                    15,
                    16,
                    17,
                    18,
                    19,
                    20,
                    21,
                    22,
                    23,
                    24,
                    25,
                    26,
                  ],
                },
              },
              executeCommand: { dynamicRegistration: true },
              configuration: true,
              workspaceFolders: true,
            },
            textDocument: {
              publishDiagnostics: {
                relatedInformation: true,
                versionSupport: false,
                tagSupport: { valueSet: [1, 2] },
              },
              synchronization: {
                dynamicRegistration: true,
                willSave: true,
                willSaveWaitUntil: true,
                didSave: true,
              },
              completion: {
                dynamicRegistration: true,
                contextSupport: true,
                completionItem: {
                  snippetSupport: true,
                  commitCharactersSupport: true,
                  documentationFormat: ["markdown", "plaintext"],
                  deprecatedSupport: true,
                  preselectSupport: true,
                  tagSupport: { valueSet: [1] },
                },
                completionItemKind: {
                  valueSet: [
                    1,
                    2,
                    3,
                    4,
                    5,
                    6,
                    7,
                    8,
                    9,
                    10,
                    11,
                    12,
                    13,
                    14,
                    15,
                    16,
                    17,
                    18,
                    19,
                    20,
                    21,
                    22,
                    23,
                    24,
                    25,
                  ],
                },
              },
              hover: {
                dynamicRegistration: true,
                contentFormat: ["markdown", "plaintext"],
              },
              signatureHelp: {
                dynamicRegistration: true,
                signatureInformation: {
                  documentationFormat: ["markdown", "plaintext"],
                  parameterInformation: { labelOffsetSupport: true },
                },
                contextSupport: true,
              },
              definition: { dynamicRegistration: true, linkSupport: true },
              references: { dynamicRegistration: true },
              documentHighlight: { dynamicRegistration: true },
              documentSymbol: {
                dynamicRegistration: true,
                symbolKind: {
                  valueSet: [
                    1,
                    2,
                    3,
                    4,
                    5,
                    6,
                    7,
                    8,
                    9,
                    10,
                    11,
                    12,
                    13,
                    14,
                    15,
                    16,
                    17,
                    18,
                    19,
                    20,
                    21,
                    22,
                    23,
                    24,
                    25,
                    26,
                  ],
                },
                hierarchicalDocumentSymbolSupport: true,
              },
              codeAction: {
                dynamicRegistration: true,
                isPreferredSupport: true,
                codeActionLiteralSupport: {
                  codeActionKind: {
                    valueSet: [
                      "",
                      "quickfix",
                      "refactor",
                      "refactor.extract",
                      "refactor.inline",
                      "refactor.rewrite",
                      "source",
                      "source.organizeImports",
                    ],
                  },
                },
              },
              codeLens: { dynamicRegistration: true },
              formatting: { dynamicRegistration: true },
              rangeFormatting: { dynamicRegistration: true },
              onTypeFormatting: { dynamicRegistration: true },
              rename: { dynamicRegistration: true, prepareSupport: true },
              documentLink: { dynamicRegistration: true, tooltipSupport: true },
              typeDefinition: { dynamicRegistration: true, linkSupport: true },
              implementation: { dynamicRegistration: true, linkSupport: true },
              colorProvider: { dynamicRegistration: true },
              foldingRange: {
                dynamicRegistration: true,
                rangeLimit: 5000,
                lineFoldingOnly: true,
              },
              declaration: { dynamicRegistration: true, linkSupport: true },
            },
          },
          initializationOptions: this.config.lsp.init || {},
          trace: "off",
          workspaceFolders: [
            {
              uri: `file://${root}`,
              name: `file://${root}`,
            },
          ],
        },
      },
    });
    await this.wait("response to lsp initialize", (msg) => {
      return (
        msg.event === "lspOutput" &&
        msg.output.id === "0d75333a-47d8-4da8-8030-c81d7bd9eed7"
      );
    });
    this.send({
      event: "lspInput",
      input: { jsonrpc: "2.0", method: "initialized", params: {} },
    });
    this.send({
      event: "lspInput",
      input: {
        jsonrpc: "2.0",
        method: "textDocument/didOpen",
        params: {
          textDocument: {
            uri: `file://${root}/${this.config.main}`,
            languageId:
              this.config.lsp.lang || this.config.monacoLang || "plaintext",
            version: 1,
            text: code,
          },
        },
      },
    });
    this.send({
      event: "lspInput",
      input: {
        jsonrpc: "2.0",
        id: "ecdb8a55-f755-4553-ae8e-91d6ebbc2045",
        method: "textDocument/completion",
        params: {
          textDocument: {
            uri: `file://${root}/${this.config.main}`,
          },
          position: findPosition(code, idx + insertedCode.length),
          context: { triggerKind: 1 },
        },
      },
    });
    const items = await this.wait(
      "response to lsp completion request",
      (msg) => {
        if (msg.event === "lspOutput") {
          if (msg.output.method === "workspace/configuration") {
            this.send({
              event: "lspInput",
              input: {
                jsonrpc: "2.0",
                id: msg.output.id,
                result: Array(msg.output.params.items.length).fill(
                  this.config.lsp.config !== undefined
                    ? this.config.lsp.config
                    : {}
                ),
              },
            });
          } else if (msg.output.id === "ecdb8a55-f755-4553-ae8e-91d6ebbc2045") {
            if (msg.output && msg.output.result) {
              return msg.output.result.items || msg.output.result;
            }
          }
        }
      }
    );
    if (!(items && items.filter(({ label }) => label === item).length > 0)) {
      throw new Error("completion item did not appear");
    }
  };
}

function lint(lang) {
  const config = langs[lang];
  if (
    config.format &&
    !config.format.input &&
    !(config.skip || []).includes("format")
  ) {
    throw new Error("formatter is missing test");
  }
  if (
    config.lsp &&
    !(config.lsp.code && config.lsp.item) &&
    !(config.skip || []).includes("lsp")
  ) {
    throw new Error("LSP is missing test");
  }
}

const testTypes = {
  ensure: {
    pred: ({ ensure }) => (ensure ? true : false),
  },
  run: { pred: (config) => true },
  repl: {
    pred: ({ repl }) => (repl ? true : false),
  },
  runrepl: {
    pred: ({ repl }) => (repl ? true : false),
  },
  scope: {
    pred: ({ scope }) => (scope ? true : false),
  },
  format: {
    pred: ({ format }) => (format ? true : false),
  },
  lsp: { pred: ({ lsp }) => (lsp ? true : false) },
};

function getTestList() {
  const tests = [];
  for (const [id, cfg] of Object.entries(langs)) {
    for (const [type, { pred }] of Object.entries(testTypes)) {
      if (pred(cfg)) {
        tests.push({ lang: id, type });
      }
    }
  }
  return tests;
}

async function writeLog(lang, type, result, log) {
  log = `${result.toUpperCase()}: ${lang}/${type}\n` + log;
  await fs.mkdir(`tests/${lang}`, { recursive: true });
  await fs.writeFile(`tests/${lang}/${type}.log`, log);
  await fs.mkdir(`tests-run/${lang}`, { recursive: true });
  await fs.symlink(
    `../../tests/${lang}/${type}.log`,
    `tests-run/${lang}/${type}.log`
  );
  await fs.mkdir(`tests-${result}/${lang}`, { recursive: true });
  await fs.symlink(
    `../../tests/${lang}/${type}.log`,
    `tests-${result}/${lang}/${type}.log`
  );
}

async function main() {
  langs = await langsPromise;
  let tests = getTestList();
  const args = process.argv.slice(2);
  if (process.env.L) {
    tests = tests.filter(({ lang }) => process.env.L.split().includes(lang));
  }
  if (process.env.T) {
    tests = tests.filter(({ type }) => process.env.T.split().includes(type));
  }
  if (tests.length === 0) {
    console.error("no tests selected");
    process.exit(1);
  }
  console.error(`Running ${tests.length} test${tests.length !== 1 ? "s" : ""}`);
  const lintSeen = new Set();
  let lintPassed = new Set();
  let lintFailed = new Map();
  for (const { lang } of tests) {
    if (!lintSeen.has(lang)) {
      lintSeen.add(lang);
      try {
        lint(lang);
        lintPassed.add(lang);
      } catch (err) {
        lintFailed.set(lang, err);
      }
    }
  }
  if (lintFailed.size > 0) {
    console.error(
      `Language${lintFailed.size !== 1 ? "s" : ""} failed linting:`
    );
    console.error(
      Array.from(lintFailed)
        .map(([lang, err]) => `  - ${lang} (${err})`)
        .join("\n")
    );
    process.exit(1);
  }
  await fs.rm("tests-run", { recursive: true, force: true });
  await fs.rm("tests-passed", { recursive: true, force: true });
  await fs.rm("tests-skipped", { recursive: true, force: true });
  await fs.rm("tests-failed", { recursive: true, force: true });
  const queue = new PQueue({ concurrency: CONCURRENCY });
  let passed = new Set();
  let skipped = new Set();
  let failed = new Map();
  for (const { lang, type } of tests) {
    queue.add(async () => {
      const test = new Test(lang, type);
      let err;
      try {
        err = await test.run();
      } catch (error) {
        err = error;
      }
      if (err === "skipped") {
        skipped.add({ lang, type });
        console.error(`SKIPPED: ${lang}/${type}`);
        await writeLog(lang, type, "skipped", "");
      } else if (!err) {
        passed.add({ lang, type });
        console.error(`PASSED: ${lang}/${type}`);
        await writeLog(
          lang,
          type,
          "passed",
          test.getLog({ pretty: true }) + "\n"
        );
      } else {
        failed.set({ lang, type }, err);
        console.error(`FAILED: ${lang}/${type}`);
        console.error(test.getLog());
        console.error(err);
        await writeLog(
          lang,
          type,
          "failed",
          test.getLog({ pretty: true }) +
            "\n" +
            (err.stack ? err.stack + "\n" : err ? `${err}` : "")
        );
      }
    });
  }
  await queue.onIdle();
  console.error();
  console.error(
    "================================================================================"
  );
  console.error();
  if (passed.size > 0) {
    console.error(`${passed.size} test${passed.size !== 1 ? "s" : ""} PASSED`);
  }
  if (skipped.size > 0) {
    console.error(
      `${skipped.size} test${skipped.size !== 1 ? "s" : ""} SKIPPED`
    );
  }
  if (failed.size > 0) {
    console.error(`${failed.size} test${failed.size !== 1 ? "s" : ""} FAILED`);
    _.sortBy(Array.from(failed), [
      ([{ lang }, _]) => lang,
      ([{ type }, _]) => type,
    ]).forEach(([{ lang, type }, err]) =>
      console.error(`  - ${lang}/${type} (${err})`)
    );
  }
  const langsValidated = {};
  passed.forEach((_, { lang }) => {
    langsValidated[lang] = true;
  });
  failed.forEach(({ lang }) => {
    langsValidated[lang] = false;
  });
  for (const [lang, validated] of Object.entries(langsValidated)) {
    if (!validated) {
      continue;
    }
    await fs.mkdir(`build/test-hashes/lang`, { recursive: true });
    await fs.writeFile(
      `build/test-hashes/lang/${lang}`,
      await getTestHash(lang, process.env.RIJU_IMAGE_HASH)
    );
  }
  process.exit(failed.size > 0 ? 1 : 0);
}

main().catch(console.error);
