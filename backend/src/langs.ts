"use strict";

export interface LangConfig {
  name: string;
  monacoLang: string;
  repl?: string;
  main?: string;
  prefix?: string;
  suffix?: string;
  compile?: string;
  run?: string;
  template?: string;
  hacks?: "ghci-config"[];
}

export const langs: { [key: string]: LangConfig } = {
  bash: {
    name: "Bash",
    monacoLang: "shell",
    repl: "bash --rcfile /dev/null",
    main: "main.bash",
    run: "bash --rcfile main.bash",
    template: `echo "Hello, world!"
`,
  },
  c: {
    name: "C",
    monacoLang: "c",
    main: "main.c",
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
    main: "main.cpp",
    compile: "clang++ -Wall -Wextra main.cpp -o main",
    run: "./main",
    template: `#include <iostream>

int main() {
  std::cout << "Hello, world!" << std::endl;
  return 0;
}
`,
  },
  clojure: {
    name: "Clojure",
    monacoLang: "clojure",
    repl: "clojure",
    main: "main.clj",
    run: "clojure -i main.clj -r",
    template: `(println "Hello, world!")
`,
  },
  emacs: {
    name: "Emacs Lisp",
    monacoLang: "plaintext",
    repl: "emacs --eval '(ielm)'",
    main: "main.el",
    run: "emacs --load main.el --eval '(ielm)'",
    template: `(message "Hello, world!")
`,
  },
  fish: {
    name: "Fish",
    monacoLang: "plaintext",
    repl: "SHELL=/usr/bin/fish fish",
    main: "main.fish",
    run: 'fish -C "$(< main.fish)"',
    template: `echo "Hello, world!"
`,
  },
  go: {
    name: "Go",
    monacoLang: "go",
    main: "main.go",
    compile: "go build main.go",
    run: "./main",
    template: `package main

import "fmt"

func main() {
	fmt.Println("Hello, world!")
}
`,
  },
  haskell: {
    name: "Haskell",
    monacoLang: "plaintext",
    repl: "ghci",
    main: "Main.hs",
    run: "ghci",
    template: `module Main where

main :: IO ()
main = putStrLn "Hello, world!"
`,
    hacks: ["ghci-config"],
  },
  java: {
    name: "Java",
    monacoLang: "java",
    main: "Main.java",
    compile: "javac Main.java",
    run: "java Main",
    template: `public class Main {
    public static void main(String[] args) {
        System.out.println("Hello, world!");
    }
}
`,
  },
  julia: {
    name: "Julia",
    monacoLang: "plaintext",
    repl: "julia",
    main: "main.jl",
    run: "julia -L main.jl",
    template: `println("Hello, world!")
`,
  },
  lua: {
    name: "Lua",
    monacoLang: "lua",
    repl: "lua",
    main: "main.lua",
    run: "lua -i main.lua",
    template: `print("Hello, world!")
`,
  },
  nodejs: {
    name: "Node.js",
    monacoLang: "javascript",
    repl: "node",
    main: "main.js",
    suffix: `
require("repl").start();
`,
    run: "node main.js",
    template: `console.log("Hello, world!")
`,
  },
  python: {
    name: "Python",
    monacoLang: "python",
    repl: "python3 -u",
    main: "main.py",
    run: "python3 -u -i main.py",
    template: `print("Hello, world!")
`,
  },
  ruby: {
    name: "Ruby",
    monacoLang: "ruby",
    repl: "irb",
    main: "main.rb",
    suffix: `
require 'irb'
IRB.setup(ARGV[0], argv: [])
workspace = IRB::WorkSpace.new(binding)
binding_irb = IRB::Irb.new(workspace)
binding_irb.run(IRB.conf)
`,
    run: "ruby main.rb",
    template: `puts "Hello, world!"
`,
  },
  rust: {
    name: "Rust",
    monacoLang: "rust",
    main: "main.rs",
    compile: "rustc main.rs",
    run: "./main",
    template: `fn main() {
    println!("Hello, world!");
}
`,
  },
  vim: {
    name: "Vimscript",
    monacoLang: "plaintext",
    repl: "vim",
    main: "main.vim",
    run: `vim -c "$(< main.vim)"`,
    template: `:echo "Hello, world!"
`,
  },
  zsh: {
    name: "Zsh",
    monacoLang: "shell",
    repl: "SHELL=/usr/bin/zsh zsh",
    main: ".zshrc",
    run: "ZDOTDIR=. zsh",
    template: `echo "Hello, world!"
`,
  },
};
