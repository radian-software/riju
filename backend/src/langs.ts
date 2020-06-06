export interface LangConfig {
  cmdline: string[];
  monacoLang: string;
  name: string;
}

export const langs = {
  bash: {
    cmdline: ["bash"],
    name: "Bash",
    monacoLang: "shell",
  },
  c: {
    cmdline: ["echo", "not implemented"],
    name: "C",
    monacoLang: "c",
  },
  "c++": {
    cmdline: ["echo", "not implemented"],
    name: "C++",
    monacoLang: "cpp",
  },
  clojure: {
    cmdline: ["clojure"],
    name: "Clojure",
    monacoLang: "clojure",
  },
  emacs: {
    cmdline: ["emacs"],
    name: "Emacs Lisp",
    monacoLang: "plaintext",
  },
  fish: {
    cmdline: ["env", "SHELL=/usr/bin/fish", "fish"],
    name: "Fish",
    monacoLang: "plaintext",
  },
  go: {
    cmdline: ["echo", "not implemented"],
    name: "Go",
    monacoLang: "go",
  },
  haskell: {
    cmdline: ["ghci"],
    name: "Haskell",
    monacoLang: "plaintext",
  },
  java: {
    cmdline: ["echo", "not implemented"],
    name: "Java",
    monacoLang: "java",
  },
  julia: {
    cmdline: ["julia"],
    name: "Julia",
    monacoLang: "plaintext",
  },
  lua: {
    cmdline: ["lua"],
    name: "Lua",
    monacoLang: "lua",
  },
  nodejs: {
    cmdline: ["node"],
    name: "Node.js",
    monacoLang: "javascript",
  },
  python: {
    cmdline: ["python3"],
    name: "Python",
    monacoLang: "python",
  },
  ruby: {
    cmdline: ["irb"],
    name: "Ruby",
    monacoLang: "ruby",
  },
  rust: {
    cmdline: ["echo", "not implemented"],
    name: "Rust",
    monacoLang: "rust",
  },
  vim: {
    cmdline: ["vim"],
    name: "Vimscript",
    monacoLang: "plaintext",
  },
  zsh: {
    cmdline: ["env", "SHELL=/usr/bin/zsh", "zsh"],
    name: "Zsh",
    monacoLang: "shell",
  },
};
