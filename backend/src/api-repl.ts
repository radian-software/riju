import * as process from "process";

import * as JSON5 from "json5";
import * as stringifyObject from "stringify-object";

import * as api from "./api";
import { langs } from "./langs";
import { mockSocket, startRepl } from "./util";

const args = process.argv.slice(2);

function die(msg: any) {
  console.error(msg);
  process.exit(1);
}

function printUsage() {
  console.log(`usage: yarn api-repl LANG`);
}

if (args.length !== 1) {
  printUsage();
  process.exit(1);
}

if (["-h", "-help", "--help", "help"].includes(args[0])) {
  printUsage();
  process.exit(0);
}

const lang = args[0];

const config = langs[lang];
if (!config) {
  console.error(`yarn api-repl: no such language: ${lang}`);
  process.exit(1);
}

const ws = mockSocket();
ws.on("send", (data: string) => {
  try {
    data = stringifyObject(JSON.parse(data), { indent: "\0" })
      .replace(/\0/g, "")
      .replace(/\n/g, " ");
  } catch (err) {
    console.error(`Invalid JSON: ${err}`);
  }
  console.log("<<< " + data + "\n");
});

startRepl({
  historyFile: ".api-repl-history",
  onLine: (line) => {
    let data;
    try {
      data = JSON5.parse(line);
    } catch (err) {
      console.error(`Invalid JSON: ${err}`);
      return;
    }
    console.log();
    ws.onMessage(JSON.stringify(data));
  },
});

const session = new api.Session(ws as any, lang, (msg) =>
  console.log(msg + "\n")
);
session.setup().catch(die);

// https://www.w3schools.com/howto/howto_js_autocomplete.asp
