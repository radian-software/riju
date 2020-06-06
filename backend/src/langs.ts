export interface LangConfig {
  repl?: string[];
  file?: string;
  run?: string[];
  monacoLang: string;
  name: string;
}

export const langs = {
  bash: {
    repl: ["bash"],
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
    name: "Zsh",
    monacoLang: "shell",
  },
};
