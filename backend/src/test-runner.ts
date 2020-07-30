import * as fs from "fs";
import * as process from "process";
import { promisify } from "util";

import PQueue from "p-queue";
import * as rimraf from "rimraf";
import { v4 as getUUID } from "uuid";

import * as api from "./api";
import { LangConfig, langs } from "./langs";

const TIMEOUT_MS = 3000;
const CONCURRENCY = 16;

function findPosition(str: string, idx: number) {
  const lines = str.substring(0, idx).split("\n");
  const line = lines.length - 1;
  const character = lines[lines.length - 1].length;
  return { line, character };
}

class Test {
  lang: string;
  type: string;
  messages: any[] = [];
  timedOut: boolean = false;
  handledMessages: number = 0;
  handleUpdate: () => void = () => {};

  get config() {
    return langs[this.lang];
  }

  ws: any = null;

  send = (msg: any) => {
    this.ws.onMessage(JSON.stringify(msg));
    this.messages.push(msg);
    this.handledMessages += 1;
  };

  constructor(lang: string, type: string) {
    this.lang = lang;
    this.type = type;
  }

  getLog = (opts?: any) => {
    opts = opts || {};
    return this.messages
      .map((msg: any) => JSON.stringify(msg, null, opts.pretty && 2))
      .join("\n");
  };

  run = async () => {
    let session = null;
    let timeout = null;
    try {
      const that = this;
      this.ws = {
        on: function (type: string, handler: any) {
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
        onMessage: function (msg: any) {
          this.messageQueue.push(msg);
        },
        messageQueue: [] as any[],
        send: function (data: string) {
          that.messages.push(JSON.parse(data));
          that.handleUpdate();
        },
        terminate: function () {},
      };
      session = new api.Session(this.ws, this.lang, (msg: string) => {
        this.messages.push({ event: "serverLog", message: msg });
      });
      timeout = setTimeout(() => {
        this.timedOut = true;
        this.handleUpdate();
      }, TIMEOUT_MS);
      await session.setup();
      switch (this.type) {
        case "ensure":
          await this.testEnsure();
          break;
        case "hello":
          await this.testHello();
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

  wait = async <T>(desc: string, handler: (msg: any) => T) => {
    return await new Promise((resolve, reject) => {
      this.handleUpdate = () => {
        if (this.timedOut) {
          reject(`timeout while waiting for ${desc}`);
        } else {
          while (this.handledMessages < this.messages.length) {
            const msg = this.messages[this.handledMessages];
            const result = handler(msg);
            if (result) {
              resolve(result);
            }
            this.handledMessages += 1;
          }
        }
      };
      this.handleUpdate();
    });
  };

  waitForOutput = async (pattern: string) => {
    let output = "";
    return await this.wait(`output ${JSON.stringify(pattern)}`, (msg: any) => {
      const prevLength = output.length;
      if (msg.event === "terminalOutput") {
        output += msg.output;
      }
      return output.indexOf(pattern, prevLength - pattern.length) != -1;
    });
  };

  testEnsure = async () => {
    this.send({ event: "ensure" });
    const code = await this.wait("ensure response", (msg: any) => {
      if (msg.event === "ensured") {
        return msg.code;
      }
    });
    if (code !== 0) {
      throw new Error(`ensure failed with code ${code}`);
    }
  };
  testHello = async () => {
    const pattern = this.config.hello || "Hello, world!";
    this.send({ event: "runCode", code: this.config.template });
    await this.waitForOutput(pattern);
  };
  testRepl = async () => {
    const input = this.config.input || "111111 + 111111";
    const output = this.config.output || "222222";
    this.send({ event: "terminalInput", input: input + "\r" });
    await this.waitForOutput(output);
  };
  testRunRepl = async () => {
    const input = this.config.input || "111111 + 111111";
    const output = this.config.output || "222222";
    this.send({ event: "runCode", code: this.config.template });
    this.send({ event: "terminalInput", input: input + "\r" });
    await this.waitForOutput(output);
  };
  testScope = async () => {
    const code = this.config.scope!.code;
    const after = this.config.scope!.after;
    const input = this.config.scope!.input || "x";
    const output = this.config.scope!.output || "222222";
    let allCode = this.config.template;
    if (!allCode.endsWith("\n")) {
      allCode += "\n";
    }
    if (after) {
      allCode = allCode.replace(after + "\n", after + "\n" + code + "\n");
    } else {
      allCode = allCode + code + "\n";
    }
    this.send({ event: "runCode", code: allCode });
    this.send({ event: "terminalInput", input: input + "\r" });
    await this.waitForOutput(output);
  };
  testFormat = async () => {
    const input = this.config.format!.input;
    const output = this.config.format!.output || this.config.template;
    this.send({ event: "formatCode", code: input });
    const result = await this.wait("formatter response", (msg: any) => {
      if (msg.event === "formattedCode") {
        return msg.code;
      }
    });
    if (output !== result) {
      throw new Error("formatted code did not match");
    }
  };
  testLsp = async () => {
    const insertedCode = this.config.lsp!.code!; // FIXME
    const after = this.config.lsp!.after;
    const item = this.config.lsp!.item!; // FIXME
    const idx = after
      ? this.config.template.indexOf(after)
      : this.config.template.length;
    const code =
      this.config.template.slice(0, idx) +
      insertedCode +
      this.config.template.slice(idx);
    const root = await this.wait("lspStarted message", (msg: any) => {
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
          initializationOptions: this.config.lsp!.init || {},
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
    await this.wait("response to lsp initialize", (msg: any) => {
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
              this.config.lsp!.lang || this.config.monacoLang || "plaintext",
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
    const items: any = await this.wait(
      "response to lsp completion request",
      (msg: any) => {
        if (msg.event === "lspOutput") {
          if (msg.output.method === "workspace/configuration") {
            this.send({
              event: "lspInput",
              input: {
                jsonrpc: "2.0",
                id: msg.output.id,
                result: Array(msg.output.params.items.length).fill(
                  this.config.lsp!.config !== undefined
                    ? this.config.lsp!.config
                    : {}
                ),
              },
            });
          } else if (msg.output.id === "ecdb8a55-f755-4553-ae8e-91d6ebbc2045") {
            return msg.output.result.items;
          }
        }
      }
    );
    if (
      !(items && items.filter(({ label }: any) => label === item).length > 0)
    ) {
      throw new Error("completion item did not appear");
    }
  };
}

function lint(lang: string) {
  const config = langs[lang];
  if (!config.template.endsWith("\n")) {
    throw new Error("template is missing a trailing newline");
  }
  // These can be removed when the types are adjusted to make these
  // situations impossible.
  if (config.format && !config.format.input) {
    throw new Error("formatter is missing test");
  }
  if (config.lsp && !(config.lsp.code && config.lsp.item)) {
    throw new Error("LSP is missing test");
  }
}

const testTypes: {
  [key: string]: {
    pred: (cfg: LangConfig) => boolean;
  };
} = {
  ensure: {
    pred: ({ ensure }) => (ensure ? true : false),
  },
  hello: { pred: (config) => true },
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
  const tests: { lang: string; type: string }[] = [];
  for (const [id, cfg] of Object.entries(langs)) {
    for (const [type, { pred }] of Object.entries(testTypes)) {
      if (pred(cfg)) {
        tests.push({ lang: id, type });
      }
    }
  }
  return tests;
}

async function writeLog(lang: string, type: string, log: string) {
  await promisify(fs.mkdir)(`tests/${lang}`, { recursive: true });
  await promisify(fs.writeFile)(`tests/${lang}/${type}.log`, log);
}

async function main() {
  let tests = getTestList();
  const args = process.argv.slice(2);
  for (const arg of args) {
    tests = tests.filter(
      ({ lang, type }) =>
        arg
          .split(",")
          .filter((arg) =>
            [lang, type].concat(langs[lang].aliases || []).includes(arg)
          ).length > 0
    );
  }
  if (tests.length === 0) {
    console.error("no tests selected");
    process.exit(1);
  }
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
  await promisify(rimraf)("tests");
  const queue = new PQueue({ concurrency: CONCURRENCY });
  let passed = new Set();
  let failed = new Map();
  for (const { lang, type } of tests) {
    let test: Test;
    queue
      .add(() => {
        test = new Test(lang, type);
        return test.run();
      })
      .then(async () => {
        passed.add({ lang, type });
        console.error(`PASSED: ${lang}/${type}`);
        await writeLog(
          lang,
          type,
          `PASSED: ${lang}/${type}\n` + test.getLog({ pretty: true }) + "\n"
        );
      })
      .catch(async (err) => {
        failed.set({ lang, type }, err);
        console.error(`FAILED: ${lang}/${type}`);
        console.error(test.getLog());
        console.error(err);
        await writeLog(
          lang,
          type,
          `FAILED: ${lang}/${type}\n` +
            test.getLog({ pretty: true }) +
            "\n" +
            err.stack +
            "\n"
        );
      })
      .catch(console.error);
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
  if (failed.size > 0) {
    console.error(`${failed.size} test${failed.size !== 1 ? "s" : ""} FAILED`);
    Array.from(failed).forEach(([{ lang, type }, err]) =>
      console.error(`  - ${lang}/${type} (${err})`)
    );
  }
  process.exit(failed.size > 0 ? 1 : 0);
}

main().catch(console.error);
