export interface LangConfig {
  repl?: string[];
  file?: string;
  prefix?: string;
  suffix?: string;
  run?: string[] | string;
  monacoLang: string;
  name: string;
}

export const langs = {
  bash: {
    repl: ["bash"],
    file: "main.bash",
    run: ["bash", "--rcfile", "main.bash"],
    name: "Bash",
    monacoLang: "shell",
  },
  c: {
    name: "C",
    monacoLang: "c",
  },
  "c++": {
    name: "C++",
    monacoLang: "cpp",
  },
  clojure: {
    repl: ["clojure"],
    name: "Clojure",
    monacoLang: "clojure",
  },
  emacs: {
    repl: ["emacs"],
    name: "Emacs Lisp",
    monacoLang: "plaintext",
  },
  fish: {
    repl: ["env", "SHELL=/usr/bin/fish", "fish"],
    name: "Fish",
    monacoLang: "plaintext",
  },
  go: {
    name: "Go",
    monacoLang: "go",
  },
  haskell: {
    repl: ["ghci"],
    file: "Main.hs",
    run: ["ghci", "Main.hs"],
    name: "Haskell",
    monacoLang: "plaintext",
  },
  java: {
    name: "Java",
    monacoLang: "java",
  },
  julia: {
    repl: ["julia"],
    name: "Julia",
    monacoLang: "plaintext",
  },
  lua: {
    repl: ["lua"],
    name: "Lua",
    monacoLang: "lua",
  },
  nodejs: {
    repl: ["node"],
    file: "main.js",
    suffix: '\n;require("repl").start();',
    run: "node main.js",
    name: "Node.js",
    monacoLang: "javascript",
  },
  python: {
    repl: ["python3", "-u"],
    file: "main.py",
    run: ["python3", "-u", "-i", "main.py"],
    name: "Python",
    monacoLang: "python",
  },
  ruby: {
    repl: ["irb"],
    name: "Ruby",
    monacoLang: "ruby",
  },
  rust: {
    name: "Rust",
    monacoLang: "rust",
  },
  vim: {
    repl: ["vim"],
    name: "Vimscript",
    monacoLang: "plaintext",
  },
  zsh: {
    repl: ["env", "SHELL=/usr/bin/zsh", "zsh"],
    file: ".zshrc",
    run: ["env", "ZDOTDIR=.", "zsh"],
    name: "Zsh",
    monacoLang: "shell",
  },
};
