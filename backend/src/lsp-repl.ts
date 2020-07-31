import * as child_process from "child_process";
import * as process from "process";
import * as nodeReadline from "readline";

import * as appRoot from "app-root-path";
import * as readline from "historic-readline";
import { quote } from "shell-quote";
import * as rpc from "vscode-jsonrpc";

import { langs } from "./langs";

const args = process.argv.slice(2);

function printUsage() {
  console.log(`usage: yarn lsp-repl (LANG | CMDLINE...)`);
}

if (args.length === 0) {
  printUsage();
  process.exit(1);
}

if (["-h", "-help", "--help", "help"].includes(args[0])) {
  printUsage();
  process.exit(0);
}

let cmdline;
if (args.length === 1 && langs[args[0]] && langs[args[0]].lsp) {
  cmdline = ["bash", "-c", langs[args[0]].lsp!.start];
} else {
  cmdline = args;
}

console.error(quote(cmdline));
const proc = child_process.spawn(cmdline[0], cmdline.slice(1));

proc.stderr.on("data", (data) => process.stderr.write(data));
proc.on("close", (code, signal) => {
  if (code) {
    console.error(`Language server exited with code ${code}`);
    process.exit(code);
  } else {
    console.error(`Language server exited due to signal ${signal}`);
    process.exit(1);
  }
});
proc.on("error", (err) => {
  console.error(`Failed to start language server: ${err}`);
  process.exit(1);
});

const reader = new rpc.StreamMessageReader(proc.stdout);
const writer = new rpc.StreamMessageWriter(proc.stdin);

reader.listen((data) => {
  console.log("<<< " + JSON.stringify(data) + "\n");
});

// https://stackoverflow.com/a/10608048/3538165
function fixStdoutFor(cli: any) {
  var oldStdout = process.stdout;
  var newStdout = Object.create(oldStdout);
  newStdout.write = function () {
    cli.output.write("\x1b[2K\r");
    var result = oldStdout.write.apply(
      this,
      (Array.prototype.slice as any).call(arguments)
    );
    cli._refreshLine();
    return result;
  };
  (process as any).__defineGetter__("stdout", function () {
    return newStdout;
  });
}

readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  path: appRoot.resolve(".lsp-repl-history"),
  next: (cli: nodeReadline.Interface) => {
    fixStdoutFor(cli);
    cli.setPrompt(">>> ");
    cli.on("line", (line: string) => {
      if (line) {
        let data;
        try {
          data = JSON.parse(line);
        } catch (err) {
          console.error(`Invalid JSON: ${err}`);
          cli.prompt();
          return;
        }
        console.log();
        writer.write(data);
      }
      cli.prompt();
    });
    cli.on("SIGINT", () => {
      console.error("^C");
      cli.write("", { ctrl: true, name: "u" });
      cli.prompt();
    });
    cli.on("close", () => {
      console.error();
      process.exit(0);
    });
    console.log();
    cli.prompt();
  },
});
