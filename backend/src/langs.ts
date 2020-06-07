export interface LangConfig {
  name: string;
  monacoLang: string;
  repl?: string;
  file?: string;
  prefix?: string;
  suffix?: string;
  compile?: string;
  run?: string;
  template?: string;
}

export const langs: { [key: string]: LangConfig } = {
  bash: {
    name: "Bash",
    monacoLang: "shell",
    repl: "bash",
    file: "main.bash",
    run: "bash --rcfile main.bash",
    template: 'echo "Hello, world!"',
  },
  c: {
    name: "C",
    monacoLang: "c",
    file: "main.c",
    compile: "clang -Wall -Wextra main.c -o main",
    run: "./main",
    template: `#include <stdio.h>

int main() {
  printf("Hello, world!\\n");
  return 0;
}
`,
  },
  "c++": {
    name: "C++",
    monacoLang: "cpp",
  },
  clojure: {
    name: "Clojure",
    monacoLang: "clojure",
    repl: "clojure",
  },
  emacs: {
    name: "Emacs Lisp",
    monacoLang: "plaintext",
    repl: "emacs",
  },
  fish: {
    name: "Fish",
    monacoLang: "plaintext",
    repl: "SHELL=/usr/bin/fish fish",
  },
  go: {
    name: "Go",
    monacoLang: "go",
  },
  haskell: {
    name: "Haskell",
    monacoLang: "plaintext",
    repl: "ghci",
    file: "Main.hs",
    run: "ghci Main.hs",
  },
  java: {
    name: "Java",
    monacoLang: "java",
  },
  julia: {
    name: "Julia",
    monacoLang: "plaintext",
    repl: "julia",
  },
  lua: {
    name: "Lua",
    monacoLang: "lua",
    repl: "lua",
  },
  nodejs: {
    name: "Node.js",
    monacoLang: "javascript",
    repl: "node",
    file: "main.js",
    suffix: '\n;require("repl").start();',
    run: "node main.js",
  },
  python: {
    name: "Python",
    monacoLang: "python",
    repl: "python3 -u",
    file: "main.py",
    run: "python3 -u -i main.py",
  },
  ruby: {
    name: "Ruby",
    monacoLang: "ruby",
    repl: "irb",
  },
  rust: {
    name: "Rust",
    monacoLang: "rust",
  },
  vim: {
    name: "Vimscript",
    monacoLang: "plaintext",
    repl: "vim",
  },
  zsh: {
    name: "Zsh",
    monacoLang: "shell",
    repl: "SHELL=/usr/bin/zsh zsh",
    file: ".zshrc",
    run: "ZDOTDIR=. zsh",
  },
};
