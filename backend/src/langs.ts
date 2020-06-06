export interface LangConfig {
  cmdline: string[];
  name: string;
}

export const langs = {
  c: {
    cmdline: ["echo", "not implemented"],
    name: "C",
  },
  "c++": {
    cmdline: ["echo", "not implemented"],
    name: "C++",
  },
  haskell: {
    cmdline: ["ghci"],
    name: "Haskell",
  },
  nodejs: {
    cmdline: ["node"],
    name: "Node.js",
  },
  python: {
    cmdline: ["python3"],
    name: "Python",
  },
};
