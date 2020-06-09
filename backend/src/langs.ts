"use strict";

export interface LangConfig {
  name: string;
  monacoLang: string;
  repl?: string;
  main: string;
  prefix?: string;
  suffix?: string;
  compile?: string;
  run: string;
  template: string;
  hacks?: "ghci-config"[];
}

export const langs: { [key: string]: LangConfig } = {
  ada: {
    name: "Ada",
    monacoLang: "plaintext",
    main: "main.adb",
    compile: "x86_64-linux-gnu-gnatmake-9 main.adb",
    run: "./main",
    template: `with Ada.Text_IO;

procedure Main is
begin
   Ada.Text_IO.Put_Line("Hello, world!");
end Main;
`,
  },
  algol: {
    name: "ALGOL 68",
    monacoLang: "plaintext",
    main: "main.alg",
    run: "a68g main.alg",
    template: `print(("Hello, world!",new line))
`,
  },
  arm: {
    name: "ARM",
    monacoLang: "plaintext",
    main: "main.S",
    compile: "arm-linux-gnueabihf-gcc main.S -o main -static",
    run: "qemu-arm-static main",
    template: `	.text
	.globl main
main:
	mov r7, #4
	mov r0, #1
	ldr r1, =message
	mov r2, #14
	swi 0
	mov r7, #1
	mov r0, #0
	swi 0
	.data
message:
	.string "Hello, world!\\n"
`,
  },
  ats: {
    name: "ATS",
    monacoLang: "postiats",
    main: "main.dats",
    compile: "patscc main.dats -o main",
    run: "./main",
    template: `val _ = print ("Hello, world!\\n")
implement main0 () = ()
`,
  },
  bash: {
    name: "Bash",
    monacoLang: "shell",
    repl: "bash --rcfile /dev/null",
    main: "main.bash",
    run: "bash --rcfile main.bash",
    template: `echo "Hello, world!"
`,
  },
  basic: {
    name: "BASIC",
    monacoLang: "plaintext",
    repl: "bwbasic",
    main: "main.bas",
    run: "bwbasic main.bas",
    template: `PRINT "Hello, world!"
`,
  },
  befunge: {
    name: "Befunge",
    monacoLang: "plaintext",
    main: "main.be",
    run: "befunge-repl main.be",
    template: `64+"!dlrow ,olleH">:#,_@
`,
  },
  brainf: {
    name: "Brainf***",
    monacoLang: "plaintext",
    repl: "brainf-repl",
    main: "main.bf",
    run: "brainf-repl main.bf",
    template: `++++++++
[
    >++++
    [
        >++
        >+++
        >+++
        >+
        <<<<-
    ]
    >+
    >+
    >-
    >>+
    [<]

    <-
]

>>.
>---.
+++++++..+++.
>>.
<-.
<.
+++.------.--------.
>>+.
>++.
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
  cmd: {
    name: "Cmd",
    monacoLang: "bat",
    repl: "wine cmd",
    main: "main.bat",
    run: `pkill wineserver64; while pgrep wineserver64 >/dev/null; do sleep 0.05; done; wine cmd /k main.bat`,
    template: `echo "Hello, world!"
`,
  },
  commonlisp: {
    name: "Common Lisp",
    monacoLang: "plaintext",
    repl: "rlwrap sbcl",
    main: "main.lisp",
    run: "rlwrap sbcl --userinit main.lisp",
    template: `(format t "Hello, world!")
`,
  },
  cpp: {
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
  crystal: {
    name: "Crystal",
    monacoLang: "plaintext",
    main: "main.cr",
    run: "crystal main.cr",
    template: `puts "Hello, world!"
`,
  },
  csharp: {
    name: "C#",
    monacoLang: "csharp",
    main: "main.cs",
    compile: "mcs main.cs",
    run: "./main.exe",
    template: `class main {
    static void Main(string[] args) {
        System.Console.WriteLine("Hello, world!");
    }
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
  clojurescript: {
    name: "ClojureScript",
    monacoLang: "clojure",
    repl: "lumo -r",
    main: "main.cljs",
    run: "lumo -i main.cljs -r",
    template: `(println "Hello, world!")
`,
  },
  cobol: {
    name: "COBOL",
    monacoLang: "plaintext",
    main: "main.cbl",
    compile: "cobc -free -x main.cbl -o main",
    run: "./main",
    template: `IDENTIFICATION DIVISION.
PROGRAM-ID. MAIN.
PROCEDURE DIVISION.
    DISPLAY "Hello, world!".
    STOP RUN.
`,
  },
  coffeescript: {
    name: "CoffeeScript",
    monacoLang: "coffee",
    repl: "coffee",
    main: "main.coffee",
    compile: "coffee -b -c main.coffee",
    run: `node -e '
eval.apply(this, [require("fs").readFileSync("main.js", {encoding: "utf-8"})])
require("/usr/lib/node_modules/coffeescript/repl").start()
'`,
    template: `console.log "Hello, world!"
`,
  },
  d: {
    name: "D",
    monacoLang: "plaintext",
    main: "main.d",
    compile: "dmd main.d",
    run: "./main",
    template: `import std.stdio;

void main() {
  writeln("Hello, world!");
}
`,
  },
  dart: {
    name: "Dart",
    monacoLang: "dart",
    main: "main.dart",
    run: "dart main.dart",
    template: `void main() {
  print('Hello, world!');
}
`,
  },
  elixir: {
    name: "Elixir",
    monacoLang: "plaintext",
    repl: "iex",
    main: "main.exs",
    run: "iex main.exs",
    template: `IO.puts("Hello, world!")
`,
  },
  elm: {
    name: "Elm",
    monacoLang: "plaintext",
    repl: "elm repl",
    main: "Main.elm",
    run: "cp /opt/elm/elm.json elm.json && run-elm Main.elm; elm repl",
    template: `module Main exposing (..)

output : String
output = "Hello, world!"
`,
  },
  elvish: {
    name: "Elvish",
    monacoLang: "plaintext",
    repl: "SHELL=/usr/bin/elvish HOME=. elvish",
    main: ".elvish/rc.elv",
    run: "SHELL=/usr/bin/elvish HOME=. elvish",
    template: `echo "Hello, world!"
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
  erlang: {
    name: "Erlang",
    monacoLang: "plaintext",
    repl: "erl",
    main: "main.erl",
    compile: "erl -compile main",
    run: "erl -s main main",
    template: `-module(main).
-export([main/0]).

main() ->
    io:fwrite("Hello, world!\\n").
`,
  },
  fish: {
    name: "Fish",
    monacoLang: "plaintext",
    repl: "SHELL=/usr/bin/fish fish",
    main: "main.fish",
    run: 'SHELL=/usr/bin/fish fish -C "$(< main.fish)"',
    template: `echo "Hello, world!"
`,
  },
  forth: {
    name: "Forth",
    monacoLang: "plaintext",
    repl: "gforth",
    main: "main.fs",
    run: "gforth main.fs",
    template: `." Hello, world!" CR
`,
  },
  fortran: {
    name: "FORTRAN",
    monacoLang: "plaintext",
    main: "main.f",
    compile: "flang main.f -o main",
    run: "./main",
    template: `       program hello
          print *, "Hello, world!"
       end program hello
`,
  },
  fsharp: {
    name: "F#",
    monacoLang: "fsharp",
    repl: "fsharpi",
    main: "main.fsx",
    run: "fsharpi --use:main.fsx",
    template: `printfn "Hello, world!"
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
  groovy: {
    name: "Groovy",
    monacoLang: "plaintext",
    repl: "groovysh",
    main: "main.groovy",
    run: "groovysh main.groovy",
    template: `print "Hello, world!";
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
  ink: {
    name: "Ink",
    monacoLang: "plaintext",
    repl: "ink",
    main: "main.ink",
    run: "ink main.ink; ink",
    template: `std := load('../../opt/ink/std')
str := load('../../opt/ink/str')

log := std.log

log('Hello, world!')
`,
  },
  intercal: {
    name: "INTERCAL",
    monacoLang: "plaintext",
    main: "main.i",
    compile: "ick main.i",
    run: "./main",
    template: `DO ,1 <- #14
PLEASE DO ,1 SUB #1 <- #238
DO ,1 SUB #2 <- #108
DO ,1 SUB #3 <- #112
DO ,1 SUB #4 <- #0
DO ,1 SUB #5 <- #64
DO ,1 SUB #6 <- #194
DO ,1 SUB #7 <- #48
PLEASE DO ,1 SUB #8 <- #22
DO ,1 SUB #9 <- #248
DO ,1 SUB #10 <- #168
DO ,1 SUB #11 <- #24
DO ,1 SUB #12 <- #16
PLEASE DO ,1 SUB #13 <- #162
DO ,1 SUB #14 <- #52
PLEASE READ OUT ,1
PLEASE GIVE UP
`,
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
  kalyn: {
    name: "Kalyn",
    monacoLang: "plaintext",
    main: "src-kalyn/Main.kalyn",
    compile: "kalyn",
    run: "out-kalyn/Main",
    template: `(import "/opt/kalyn/Stdlib.kalyn")

(public def main (IO Empty)
  (print "Hello, world!\\n"))
`,
  },
  kotlin: {
    name: "Kotlin",
    monacoLang: "kotlin",
    repl: "kotlinc",
    main: "main.kts",
    run: "kotlinc -script main.kts; kotlinc",
    template: `println("Hello, world!")
`,
  },
  ksh: {
    name: "Ksh",
    monacoLang: "shell",
    repl: "SHELL=/usr/bin/ksh HOME=. ksh",
    main: ".kshrc",
    run: "SHELL=/usr/bin/ksh HOME=. ksh",
    template: `echo "Hello, world!"
`,
  },
  lolcode: {
    name: "LOLCODE",
    monacoLang: "plaintext",
    main: "main.lol",
    run: "lci main.lol",
    template: `HAI 1.2
  VISIBLE "Hello, world!"
KTHXBYE
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
  malbolge: {
    name: "Malbolge",
    monacoLang: "plaintext",
    main: "main.mb",
    run: "malbolge main.mb",
    template:
      " (=<`#9]~6ZY32Vx/4Rs+0No-&Jk)\"Fh}|Bcy?`=*z]Kw%oG4UUS0/@-ejc(:'8dc\n",
  },
  mips: {
    name: "MIPS",
    monacoLang: "mips",
    main: "main.S",
    compile: "mips64-linux-gnuabi64-gcc main.S -o main -static",
    run: "qemu-mips64-static main",
    template: `	.text
	.global main
main:
	li $v0, 5001
	li $a0, 1
	dla $a1, message
	li $a2, 14
	syscall
	li $v0, 5058
	li $a0, 0
	syscall
	.data
message:
	.string "Hello, world!\\n"
`,
  },
  mumps: {
    name: "MUMPS",
    monacoLang: "plaintext",
    main: "main.m",
    run:
      "gtm_dist=/usr/lib/x86_64-linux-gnu/fis-gtm/V6.3-007_x86_64 /usr/lib/x86_64-linux-gnu/fis-gtm/V6.3-007_x86_64/utf8/mumps -r main main.m",
    template: `main()
  write "Hello, world!",!
  quit
`,
  },
  nim: {
    name: "Nim",
    monacoLang: "plaintext",
    main: "main.nim",
    compile: "nim compile main.nim",
    run: "./main",
    template: `echo "Hello, world!"
`,
  },
  nodejs: {
    name: "Node.js",
    monacoLang: "javascript",
    repl: "node",
    main: "main.js",
    run: `node -e '
eval.apply(this, [require("fs").readFileSync("main.js", {encoding: "utf-8"})])
require("repl").start()
'`,
    template: `console.log("Hello, world!")
`,
  },
  objectivec: {
    name: "Objective-C",
    monacoLang: "objective-c",
    main: "main.m",
    compile:
      "gcc $(gnustep-config --objc-flags) main.m $(gnustep-config --base-libs) -o main",
    run: "./main",
    template: `#import <Foundation/Foundation.h>

int main() {
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  NSLog(@"Hello, world!");
  [pool drain];
  return 0;
}
`,
  },
  octave: {
    name: "Octave",
    monacoLang: "plaintext",
    repl: "octave",
    main: "main.m",
    run: "octave --persist main.m",
    template: `disp("Hello, world!")
`,
  },
  pascal: {
    name: "Pascal",
    monacoLang: "pascal",
    main: "main.pas",
    compile: "fpc main.pas",
    run: "./main",
    template: `program Main;
begin
   writeln('Hello, world!');
end.
`,
  },
  perl: {
    name: "Perl",
    monacoLang: "perl",
    repl: "re.pl",
    main: "main.pl",
    run: "re.pl --rcfile ./main.pl",
    template: `print("Hello, world!\\n")
`,
  },
  php: {
    name: "PHP",
    monacoLang: "php",
    repl: "php -a",
    main: "main.php",
    run: "php -d auto_prepend_file=main.php -a",
    template: `<?php

echo "Hello, world!\\n";
`,
  },
  powershell: {
    name: "PowerShell",
    monacoLang: "powershell",
    repl: "SHELL=/usr/bin/pwsh pwsh",
    main: "main.ps1",
    run: "SHELL=/usr/bin/pwsh pwsh -NoExit main.ps1",
    template: `Write-Host "Hello, world!"
`,
  },
  prolog: {
    name: "Prolog",
    monacoLang: "plaintext",
    repl: "prolog",
    main: "main.pl",
    run: "prolog main.pl",
    template: `:- initialization main.

main :-
    write("Hello, world!"), nl.
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
  r: {
    name: "R",
    monacoLang: "r",
    repl: "R",
    main: ".Rprofile",
    run: "R --no-save",
    template: `print("Hello, world!")
`,
  },
  racket: {
    name: "Racket",
    monacoLang: "plaintext",
    repl: "racket",
    main: "main.rkt",
    run: `racket -i -e '(enter! "main.rkt") (display "[ type (enter! \\"main.rkt\\") to access local variables ]\\n")'`,
    template: `#lang racket/base
(display "Hello, world!\\n")
`,
  },
  reasonml: {
    name: "ReasonML",
    monacoLang: "plaintext",
    main: "main.re",
    compile: "bsc main.re > main.js",
    run: "NODE_PATH=/usr/lib/node_modules node main.js",
    template: `print_string("Hello, world!\\n")
`,
  },
  riscv: {
    name: "RISC-V",
    monacoLang: "plaintext",
    main: "main.S",
    compile: "riscv64-linux-gnu-gcc main.S -o main -static",
    run: "qemu-riscv64-static main",
    template: `	.text
	.global main
main:
	addi a7, x0, 64
	addi a0, x0, 1
	la a1, message
	addi a2, x0, 14
	ecall
	addi a7, x0, 93
	addi a0, x0, 0
	ecall
	.data
message:
	.string "Hello, world!\\n"
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
  scala: {
    name: "Scala",
    monacoLang: "plaintext",
    repl: "scala",
    main: "main.scala",
    run: "scala -i main.scala",
    template: `println("Hello, world!")
`,
  },
  scheme: {
    name: "Scheme",
    monacoLang: "scheme",
    repl: "mit-scheme",
    main: "main.scm",
    run: "mit-scheme --load main.scm",
    template: `(begin
  (display "Hello, world!")
  (newline))
`,
  },
  sh: {
    name: "Sh",
    monacoLang: "shell",
    repl: "SHELL=/usr/bin/sh HOME=. posh -l",
    main: ".profile",
    run: "SHELL=/usr/bin/sh HOME=. posh -l",
    template: `echo "Hello, world!"
`,
  },
  shakespeare: {
    name: "Shakespeare",
    monacoLang: "plaintext",
    repl: "shakespeare console",
    main: "main.spl",
    suffix: "\n[A pause]",
    run: "shakespeare debug main.spl",
    template: `The Infamous Hello World Program.

Romeo, a young man with a remarkable patience.
Juliet, a likewise young woman of remarkable grace.
Ophelia, a remarkable woman much in dispute with Hamlet.
Hamlet, the flatterer of Andersen Insulting A/S.


                    Act I: Hamlet's insults and flattery.

                    Scene I: The insulting of Romeo.

[Enter Hamlet and Romeo]

Hamlet:
 You lying stupid fatherless big smelly half-witted coward!
 You are as stupid as the difference between a handsome rich brave
 hero and thyself! Speak your mind!

 You are as brave as the sum of your fat little stuffed misused dusty
 old rotten codpiece and a beautiful fair warm peaceful sunny summer's
 day. You are as healthy as the difference between the sum of the
 sweetest reddest rose and my father and yourself! Speak your mind!

 You are as cowardly as the sum of yourself and the difference
 between a big mighty proud kingdom and a horse. Speak your mind.

 Speak your mind!

[Exit Romeo]

                    Scene II: The praising of Juliet.

[Enter Juliet]

Hamlet:
 Thou art as sweet as the sum of the sum of Romeo and his horse and his
 black cat! Speak thy mind!

[Exit Juliet]

                    Scene III: The praising of Ophelia.

[Enter Ophelia]

Hamlet:
 Thou art as lovely as the product of a large rural town and my amazing
 bottomless embroidered purse. Speak thy mind!

 Thou art as loving as the product of the bluest clearest sweetest sky
 and the sum of a squirrel and a white horse. Thou art as beautiful as
 the difference between Juliet and thyself. Speak thy mind!

[Exeunt Ophelia and Hamlet]


                    Act II: Behind Hamlet's back.

                    Scene I: Romeo and Juliet's conversation.

[Enter Romeo and Juliet]

Romeo:
 Speak your mind. You are as worried as the sum of yourself and the
 difference between my small smooth hamster and my nose. Speak your
 mind!

Juliet:
 Speak YOUR mind! You are as bad as Hamlet! You are as small as the
 difference between the square of the difference between my little pony
 and your big hairy hound and the cube of your sorry little
 codpiece. Speak your mind!

[Exit Romeo]

                    Scene II: Juliet and Ophelia's conversation.

[Enter Ophelia]

Juliet:
 Thou art as good as the quotient between Romeo and the sum of a small
 furry animal and a leech. Speak your mind!

Ophelia:
 Thou art as disgusting as the quotient between Romeo and twice the
 difference between a mistletoe and an oozing infected blister! Speak
 your mind!

[Exeunt]
`,
  },
  smalltalk: {
    name: "Smalltalk",
    monacoLang: "plaintext",
    repl: "gst",
    main: "main.st",
    run: "gst main.st; gst",
    template: `'Hello, world!' displayNl !
`,
  },
  snobol: {
    name: "SNOBOL",
    monacoLang: "plaintext",
    repl: "snobol4",
    main: "main.sno",
    run: "snobol4 main.sno; snobol4",
    template: ` OUTPUT = "Hello, world!"
END
`,
  },
  sqlite: {
    name: "SQLite",
    monacoLang: "sql",
    repl: "sqlite3",
    main: "main.sql",
    run: `sqlite3 -cmd "$(< main.sql)"`,
    template: `SELECT "Hello, world!"
`,
  },
  standardml: {
    name: "Standard ML",
    monacoLang: "plaintext",
    repl: "rlwrap sml",
    main: "main.sml",
    run: "rlwrap sml main.sml",
    template: `print "Hello, world!\\n";
`,
  },
  swift: {
    name: "Swift",
    monacoLang: "swift",
    main: "main.swift",
    compile: "swiftc main.swift",
    run: "./main",
    template: `print("Hello, world!")
`,
  },
  tcl: {
    name: "Tcl",
    monacoLang: "tcl",
    repl: "tclsh",
    main: ".tclshrc",
    run: "HOME=. tclsh",
    template: `puts {Hello, world!}
`,
  },
  tcsh: {
    name: "Tcsh",
    monacoLang: "shell",
    repl: "SHELL=/usr/bin/tcsh HOME=. tcsh",
    main: ".tcshrc",
    run: "SHELL=/usr/bin/tcsh HOME=. tcsh",
    template: `echo "Hello, world!"
`,
  },
  typescript: {
    name: "TypeScript",
    monacoLang: "typescript",
    repl: "ts-node",
    main: "main.ts",
    run: `ts-node -i -e "$(< main.ts)"`,
    template: `console.log("Hello, world!");
`,
  },
  unlambda: {
    name: "Unlambda",
    monacoLang: "plaintext",
    repl: "unlambda-repl",
    main: "main.unl",
    run: "unlambda-repl main.unl",
    template: "`.\n`.!`.d`.l`.r`.o`.w`. `.,`.o`.l`.l`.e`.Hi\n",
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
  visualbasic: {
    name: "Visual Basic",
    monacoLang: "vb",
    main: "main.vb",
    compile: "vbnc main.vb",
    run: "mono main.exe",
    template: `Module Main
    Sub Main(args As String())
        Console.WriteLine("Hello, world!")
    End Sub
End Module
`,
  },
  whitespace: {
    name: "Whitespace",
    monacoLang: "plaintext",
    main: "main.ws",
    run: "whitespace main.ws",
    template: `Hello, world!  \t  \t   \n\t\n     \t\t  \t \t\n\t\n     \t\t \t\t  \n\t\n     \t\t \t\t  \n\t\n     \t\t \t\t\t\t\n\t\n     \t \t\t  \n\t\n     \t     \n\t\n     \t\t\t \t\t\t\n\t\n     \t\t \t\t\t\t\n\t\n     \t\t\t  \t \n\t\n     \t\t \t\t  \n\t\n     \t\t  \t  \n\t\n  \n\n\n`,
  },
  wolframlanguage: {
    name: "Wolfram Language",
    monacoLang: "plaintext",
    repl: "mathics",
    main: "main.wls",
    run: "mathics --persist main.wls",
    template: `Print["Hello, world!"]
`,
  },
  x86: {
    name: "x86",
    monacoLang: "plaintext",
    main: "main.S",
    compile: "clang main.S -o main",
    run: "./main",
    template: `	.text
	.globl main
main:
	movq $1, %rax
	movq $1, %rdi
	leaq message(%rip), %rsi
	movq $14, %rdx
	syscall
	movq $60, %rax
	movq $0, %rdi
	syscall
	.data
message:
	.string "Hello, world!\\n"
`,
  },
  zsh: {
    name: "Zsh",
    monacoLang: "shell",
    repl: "SHELL=/usr/bin/zsh zsh",
    main: ".zshrc",
    run: "SHELL=/usr/bin/zsh ZDOTDIR=. zsh",
    template: `echo "Hello, world!"
`,
  },
};
