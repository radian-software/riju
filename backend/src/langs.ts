export interface LangConfig {
  cmdline: string[];
  name: string;
}

export const langs = {
  bash: {
    cmdline: ["bash"],
    name: "Bash",
  },
  c: {
    cmdline: ["echo", "not implemented"],
    name: "C",
  },
  "c++": {
    cmdline: ["echo", "not implemented"],
    name: "C++",
  },
  clojure: {
    cmdline: ["clojure"],
    name: "Clojure",
  },
  emacs: {
    cmdline: ["emacs"],
    name: "Emacs Lisp",
  },
  fish: {
    cmdline: ["env", "SHELL=/usr/bin/fish", "fish"],
    name: "Fish",
  },
  go: {
    cmdline: ["echo", "not implemented"],
    name: "Go",
  },
  haskell: {
    cmdline: ["ghci"],
    name: "Haskell",
  },
  java: {
    cmdline: ["echo", "not implemented"],
    name: "Java",
  },
  julia: {
    cmdline: ["julia"],
    name: "Julia",
  },
  lua: {
    cmdline: ["lua"],
    name: "Lua",
  },
  nodejs: {
    cmdline: ["node"],
    name: "Node.js",
  },
  python: {
    cmdline: ["python3"],
    name: "Python",
  },
  ruby: {
    cmdline: ["irb"],
    name: "Ruby",
  },
  rust: {
    cmdline: ["echo", "not implemented"],
    name: "Rust",
  },
  vim: {
    cmdline: ["vim"],
    name: "Vimscript",
  },
  zsh: {
    cmdline: ["env", "SHELL=/usr/bin/zsh", "zsh"],
    name: "Zsh",
  },
};
