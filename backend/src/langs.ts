export interface LangConfig {
  aliases?: string[];
  name: string;
  monacoLang?: string;
  daemon?: string;
  setup?: string;
  repl?: string;
  main: string;
  prefix?: string;
  suffix?: string;
  createEmpty?: string;
  compile?: string;
  run: string;
  format?: string;
  pkg?: {
    install: string;
    uninstall?: string;
    all?: string;
    search?: string;
  };
  lspSetup?: string;
  lsp?: string;
  lspDisableDynamicRegistration?: boolean;
  lspInit?: any;
  lspConfig?: any;
  lspLang?: string;
  template: string;
  test?: {
    ensure?: string;
  };
}

export const langs: { [key: string]: LangConfig } = {
  "><>": {
    aliases: ["esofish"],
    name: "><>",
    main: "main.fish",
    run: "esofish main.fish",
    template: `"Hello, world!"r\\
           o;!?l<
`,
  },
  ada: {
    aliases: ["adb"],
    name: "Ada",
    main: "main.adb",
    compile: "x86_64-linux-gnu-gnatmake-9 main.adb",
    run: "./main",
    lsp: "ada_language_server",
    template: `with Ada.Text_IO;

procedure Main is
begin
   Ada.Text_IO.Put_Line("Hello, world!");
end Main;
`,
  },
  algol: {
    aliases: ["alg"],
    name: "ALGOL 68",
    main: "main.alg",
    run: "a68g main.alg",
    template: `print(("Hello, world!",new line))
`,
  },
  apl: {
    name: "APL",
    repl: "apl",
    main: "main.apl",
    run: "apl -f main.apl",
    template: `'Hello, world!'
`,
  },
  arm: {
    name: "ARM",
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
  asciidoc: {
    aliases: ["adoc", "asc"],
    name: "AsciiDoc",
    main: "main.adoc",
    compile: "asciidoc -s main.adoc",
    run: "prettier --no-config main.html",
    template: `Hello, world!
`,
  },
  ats: {
    aliases: ["dats"],
    name: "ATS",
    monacoLang: "postiats",
    main: "main.dats",
    compile: "patscc main.dats -o main",
    run: "./main",
    template: `val _ = print ("Hello, world!\\n")
implement main0 () = ()
`,
  },
  awk: {
    aliases: ["gawk", "mawk"],
    name: "Awk",
    main: "main.awk",
    run: `awk -f main.awk`,
    template: `BEGIN { print "Hello, world!" }
`,
  },
  bash: {
    aliases: ["bashrc", "bourneshell"],
    name: "Bash",
    monacoLang: "shell",
    repl: "bash --rcfile /dev/null",
    main: "main.bash",
    run: "bash --rcfile main.bash",
    lsp: "bash-language-server start",
    template: `echo "Hello, world!"
`,
  },
  basic: {
    aliases: ["bas", "qbasic"],
    name: "BASIC",
    repl: "bwbasic",
    main: "main.bas",
    run: "bwbasic main.bas",
    template: `PRINT "Hello, world!"
`,
  },
  beatnik: {
    name: "Beatnik",
    main: "main.beatnik",
    run: "beatnik main.beatnik",
    template: `Soars, larkspurs, rains.
Indistinctness.
Mario snarl (nurses, natures, rules...) sensuously retries goal.
Agribusinesses' costs par lain ropes (mopes) autos' cores.
Tuner ambitiousness.
Flit.
Dour entombment.
Legals' saner kinking lapse.
Nests glint.
Dread, tied futures, dourer usual tumor grunts alter atonal
  garb tries shouldered coins.
Taste a vast lustiness.
Stile stuns gad subgroup gram lanes.
Draftee insurer road: cuckold blunt, strut sunnier.
Rely enure pantheism: arty gain groups (genies, pan) titters, tattles, nears.
Bluffer tapes?  Idle diatom stooge!
Feted antes anklets ague?  Remit goiter gout!
Doubtless teared toed alohas will dull gangs' aerials' tails' sluices;
Gusset ends!  Gawkier halo!

Enter abstruse rested loser beer guy louts.
Curtain roams lasso weir lupus stunt.
Truant bears animate talon.  Entire torte originally timer.
Redo stilt gobs.

Utter centaurs;
Urgent stars;
Usurers (dilute);
Noses;
Bones;
Brig sonar graders;
Utensil silts;
Lazies.
Fret arson veterinary rows.

Atlas grunted: "Pates, slues, sulfuric manor liaising tines,
  trailers, rep... unfair!  Instant snots!"

Sled rested until eatery fail.
Ergs fortitude
  Indent spotter
Euros enter egg.
Curious tenures.
Torus cutlasses.
Sarong torso earns cruel lags it reeled.

Engineer: "Erase handbag -- unite ratification!"

oaring oaten donkeys unsold, surer rapid saltest tags
BUTTERED TIBIA LUGS REWIRING TOILETS
anion festers raring edit epilogues.
DIRGE ROTOR.
linnet oaring.
GORE BOOTIES.
Ironed goon lists tallest sublets --
Riots,
Raucous onset.

Ignobly, runners' diet anguishes sunrise loner.
Erode mob, slier switcher!
Loaners stilt drudge pearl atoll, risking hats' ends.

Rebind sitters.

Toga epistles -- crud lard.  (Pager purse dons souls.)

glob title a curio hired rites shed suds lade grease strut arctic revs toad
unless idlers rind stilt region land GERMICIDES SULTANA GUTS gill siting leans
nice spurs
tests gloves
roused asp

Holes!  Moles!  (Sores!)
Hygienists!  Scars!  (Asses!)
Smells spell rares.

Cubs instant sing in parse goodies.
Rosin.  Unhelpful sisal acres.  Slope told.
MALENESS PASTA LAB.  "Infirmary vine," rang illiterates (beans).
Rosin sours, insults truss abalones, nailed rules, helical atlases.
Dear remodeling stings mar rents.
Sunless shiner orb (silly idol.)
Clarity disses senna.
Vagabonds sauted; sloes performed gelds.
Alter post radial lip sectioning gums.
Saint Towellings.
Larger aeons telephone stolid char, pal!
Boats Dean forsook, rosters, tunas, terrariums -- united, traced.
Nude pagoda careens.
`,
  },
  befunge: {
    aliases: ["be"],
    name: "Befunge",
    main: "main.be",
    run: "befunge-repl main.be",
    template: `64+"!dlrow ,olleH">:#,_@
`,
  },
  blc: {
    aliases: [
      "binarylambdacalculus",
      "lc",
      "binary",
      "lambdacalculus",
      "lambda",
    ],
    name: "Binary Lambda Calculus",
    main: "main.blc",
    run: "cat main.blc | binary-to-text | tromp",
    template: `001010100100100001100101011011000110110001101111001011000010
000001110111011011110111001001101100011001000010000100001010
`,
  },
  brainf: {
    aliases: ["brainfuck", "bf"],
    name: "Brainf***",
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
    aliases: ["gcc", "clang", "h", "cc", "c99", "c11", "c18"],
    name: "C",
    monacoLang: "c",
    main: "main.c",
    compile: "clang -Wall -Wextra main.c -o main",
    run: "./main",
    lspSetup: `echo '-Wall -Wextra' | sed -E 's/\\s+/\\n/g' > compile_flags.txt`,
    lsp: "clangd",
    template: `#include <stdio.h>

int main() {
  printf("Hello, world!\\n");
  return 0;
}
`,
  },
  ceylon: {
    name: "Ceylon",
    main: "source/main.ceylon",
    run: `PATH="/usr/lib/jvm/java-8-openjdk-amd64/bin:$PATH" ceylon run --compile=force default`,
    template: `shared void run() {
    print("Hello, world!");
}
`,
  },
  chef: {
    name: "Chef",
    main: "main.chef",
    run: "chef main.chef",
    template: `Hello World Cake with Chocolate Sauce.

Ingredients.
33 g chocolate chips
100 g butter
54 ml double cream
2 pinches baking powder
114 g sugar
111 ml beaten eggs
119 g flour
32 g cocoa powder
0 g cake mixture

Cooking time: 25 minutes.

Pre-heat oven to 180 degrees Celsius.

Method.
Put chocolate chips into the mixing bowl.
Put butter into the mixing bowl.
Put sugar into the mixing bowl.
Put beaten eggs into the mixing bowl.
Put flour into the mixing bowl.
Put baking powder into the mixing bowl.
Put cocoa  powder into the mixing bowl.
Stir the mixing bowl for 1 minute.
Combine double cream into the mixing bowl.
Stir the mixing bowl for 4 minutes.
Liquefy the contents of the mixing bowl.
Pour contents of the mixing bowl into the baking dish.
bake the cake mixture.
Wait until baked.
Serve with chocolate sauce.

Chocolate Sauce.

Ingredients.
111 g sugar
108 ml hot water
108 ml heated double cream
101 g dark chocolate
72 g milk chocolate

Method.
Clean the mixing bowl.
Put sugar into the mixing bowl.
Put hot water into the mixing bowl.
Put heated double cream into the mixing bowl.
dissolve the sugar.
agitate the sugar until dissolved.
Liquefy the dark chocolate.
Put dark chocolate into the mixing bowl.
Liquefy the milk chocolate.
Put milk chocolate into the mixing bowl.
Liquefy contents of the mixing bowl.
Pour contents of the mixing bowl into the baking dish.
Refrigerate for 1 hour.
`,
  },
  cmd: {
    aliases: ["bat", "batch", "wine"],
    name: "Cmd",
    monacoLang: "bat",
    repl: "wine cmd",
    main: "main.bat",
    run: `wine cmd /k main.bat`,
    template: `echo "Hello, world!"
`,
  },
  commonlisp: {
    aliases: ["lisp", "sbcl"],
    name: "Common Lisp",
    repl: "rlwrap sbcl",
    main: "main.lisp",
    run: "rlwrap sbcl --userinit main.lisp",
    template: `(format t "Hello, world!")
`,
  },
  confluence: {
    aliases: ["jira", "atlassian"],
    name: "Confluence",
    main: "main.txt",
    compile: "pandoc main.txt -f jira -o main.html",
    run: "prettier --no-config main.html",
    template: `Hello, world!
`,
  },
  "c++": {
    aliases: [
      "cpp",
      "g++",
      "clang++",
      "c++98",
      "c++03",
      "c++11",
      "c++14",
      "c++17",
      "c++20",
      "cpp98",
      "cpp03",
      "cpp11",
      "cpp14",
      "cpp17",
      "cpp20",
      "hpp",
      "cxx",
      "hxx",
    ],
    name: "C++",
    monacoLang: "cpp",
    main: "main.cpp",
    compile: "clang++ -Wall -Wextra main.cpp -o main",
    run: "./main",
    lspSetup: `echo '-Wall -Wextra' | sed -E 's/\\s+/\\n/g' > compile_flags.txt`,
    lsp: "clangd",
    template: `#include <iostream>

int main() {
  std::cout << "Hello, world!" << std::endl;
  return 0;
}
`,
  },
  crystal: {
    aliases: ["cr"],
    name: "Crystal",
    main: "main.cr",
    run: "crystal main.cr",
    template: `puts "Hello, world!"
`,
  },
  csharp: {
    aliases: ["cs", "mcs"],
    name: "C#",
    monacoLang: "csharp",
    main: "main.cs",
    compile: "mcs main.cs",
    run: "mono main.exe",
    template: `class main {
    static void Main(string[] args) {
        System.Console.WriteLine("Hello, world!");
    }
}
`,
  },
  clojure: {
    aliases: ["clj"],
    name: "Clojure",
    monacoLang: "clojure",
    repl: "clojure",
    main: "main.clj",
    run: "clojure -i main.clj -r",
    lsp: "clojure-lsp",
    template: `(println "Hello, world!")
`,
  },
  clojurescript: {
    aliases: ["cljs", "lumo"],
    name: "ClojureScript",
    monacoLang: "clojure",
    repl: "lumo -r",
    main: "main.cljs",
    run: "lumo -i main.cljs -r",
    template: `(println "Hello, world!")
`,
  },
  cobol: {
    aliases: ["cbl", "cobc"],
    name: "COBOL",
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
    aliases: ["coffee"],
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
    aliases: ["dmd"],
    name: "D",
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
  dogescript: {
    aliases: ["doge", "ds", "wow"],
    name: "Dogescript",
    repl: "dogescript",
    main: "main.djs",
    run: "dogescript main.djs | node; dogescript",
    template: `plz console.loge with "Hello, world!"
`,
  },
  dhall: {
    name: "Dhall",
    main: "main.dhall",
    compile: "cat main.dhall | dhall-to-json > main.json",
    run: "cat main.json | jq .",
    template: `{ output = "Hello, world!" }
`,
  },
  dokuwiki: {
    aliases: ["doku"],
    name: "DokuWiki",
    main: "main.txt",
    compile: "pandoc main.txt -f dokuwiki -o main.html",
    run: "prettier --no-config main.html",
    template: `Hello, world!
`,
  },
  elixir: {
    aliases: ["iex", "exs"],
    name: "Elixir",
    repl: "iex",
    main: "main.exs",
    run: "iex main.exs",
    lsp: "/opt/elixir-ls/language_server.sh",
    template: `IO.puts("Hello, world!")
`,
  },
  elm: {
    name: "Elm",
    repl: "elm repl",
    main: "Main.elm",
    run: "cp /opt/elm/elm.json elm.json && run-elm Main.elm; elm repl",
    lsp: "elm-language-server --stdio",
    lspSetup: "cp /opt/elm/elm.json elm.json",
    template: `module Main exposing (..)

output : String
output = "Hello, world!"
`,
  },
  elvish: {
    aliases: ["elv"],
    name: "Elvish",
    repl: `SHELL=/usr/bin/elvish HOME="$PWD" elvish`,
    main: ".elvish/rc.elv",
    createEmpty: ``,
    run: `SHELL=/usr/bin/elvish HOME="$PWD" elvish`,
    template: `echo "Hello, world!"
`,
  },
  emacs: {
    aliases: ["emacslisp", "elisp", "gnuemacs", "xemacs", "ielm"],
    name: "Emacs Lisp",
    repl: `emacs --eval "(progn (require 'package) (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) (package-initialize) (ielm))"`,
    main: "main.el",
    run: `emacs --load main.el --eval "(progn (require 'package) (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) (package-initialize) (ielm))"`,
    pkg: {
      install: `emacs -Q --batch --eval "(progn (require 'package) (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) (package-initialize) (unless (ignore-errors (>= (length (directory-files \"~/.emacs.d/elpa/archives\")) 4)) (package-refresh-contents)) (package-install 'NAME))"`,
      uninstall: `ls ~/.emacs.d/elpa | grep -- - | grep '^NAME-[0-9]' | while read pkg; do emacs -Q --batch --eval "(progn (require 'package) (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) (package-initialize) (unless (ignore-errors (>= (length (directory-files \"~/.emacs.d/elpa/archives\")) 4)) (package-refresh-contents)) (call-interactively 'package-delete))" <<< "$pkg"; done`,
      all: `set -o pipefail; (curl -sS https://elpa.gnu.org/packages/ | grep '<td>' | grep -Eo '[^>]+</a>' | grep -Eo '^[^<]+' && curl -sS https://melpa.org/archive.json | jq -r 'keys | .[]') | sort | uniq`,
    },
    template: `(message "Hello, world!")
`,
  },
  emojicode: {
    aliases: ["emoji", "emojic", "emojicodec"],
    name: "Emojicode",
    main: "main.emojic",
    compile: "emojicodec main.emojic",
    run: "./main",
    template: `🏁 🍇
  😀 🔤Hello, world!🔤❗️
🍉
`,
  },
  entropy: {
    aliases: ["ent", "entc", "vge"],
    name: "Entropy",
    main: "main.vge",
    compile: `mono /opt/entropy/entc.exe main.vge | grep -Ev 'WARNING:|Using default' > main.cs && mcs -lib:/opt/entropy -r:Rottytooth.Esolang.Entropy main.cs`,
    run: "MONO_PATH=/opt/entropy mono main.exe",
    template: `Program MyNamespace MyProgram [
	print "Hello, world!";
]
`,
  },
  erlang: {
    aliases: ["erl"],
    name: "Erlang",
    repl: "erl",
    main: "main.erl",
    compile: "erl -compile main",
    run: "erl -s main main",
    lsp: "erlang_ls",
    template: `-module(main).
-export([main/0]).

main() ->
    io:fwrite("Hello, world!\\n").
`,
  },
  euphoria: {
    aliases: ["ex", "exw", "exu", "euc", "eui", "eub"],
    name: "Euphoria",
    main: "main.exu",
    run: "eui main.exu",
    template: `puts(1, "Hello, world!\\n")
`,
  },
  factor: {
    aliases: ["fact"],
    name: "Factor",
    repl: "factor-lang",
    main: ".factor-rc",
    createEmpty: ``,
    run: "factor-lang",
    template: `USE: io

"Hello, world!" print
`,
  },
  fish: {
    name: "Fish",
    repl: "SHELL=/usr/bin/fish fish",
    main: "main.fish",
    run: 'SHELL=/usr/bin/fish fish -C "$(< main.fish)"',
    template: `echo "Hello, world!"
`,
  },
  forth: {
    aliases: ["fs", "gforth"],
    name: "Forth",
    repl: "gforth",
    main: "main.fs",
    run: "gforth main.fs",
    template: `." Hello, world!" CR
`,
  },
  fortran: {
    aliases: [
      "f",
      "flang",
      "fortran77",
      "fortran90",
      "fortran95",
      "fortran2003",
      "fortran2008",
    ],
    name: "FORTRAN",
    main: "main.f",
    compile: "flang main.f -o main",
    run: "./main",
    lsp: "fortls",
    template: `       program hello
          print *, "Hello, world!"
       end program hello
`,
  },
  fsharp: {
    aliases: ["fsharpi", "fsx", "fs"],
    name: "F#",
    monacoLang: "fsharp",
    repl: "fsharpi",
    main: "main.fsx",
    run: "fsharpi --use:main.fsx",
    template: `printfn "Hello, world!"
`,
  },
  go: {
    aliases: ["golang"],
    name: "Go",
    monacoLang: "go",
    main: "main.go",
    compile: "go build main.go",
    run: "./main",
    format: "cat main.go | gofmt",
    lsp: "gopls",
    template: `package main

import "fmt"

func main() {
	fmt.Println("Hello, world!")
}
`,
  },
  golfscript: {
    aliases: ["gs", "golf"],
    name: "GolfScript",
    main: "main.gs",
    run: "golfscript main.gs",
    template: `'Hello, world!'
`,
  },
  groovy: {
    name: "Groovy",
    repl: `JAVA_OPTS="-Djava.util.prefs.systemRoot=$PWD/.java -Djava.util.prefs.userRoot=$PWD/.java/.userPrefs" groovysh`,
    main: "main.groovy",
    run: `JAVA_OPTS="-Djava.util.prefs.systemRoot=$PWD/.java -Djava.util.prefs.userRoot=$PWD/.java/.userPrefs" groovysh main.groovy`,
    template: `print "Hello, world!";
`,
  },
  hack: {
    name: "Hack",
    repl: "hhvm -a",
    main: "main.hack",
    run: "hhvm -a main.hack",
    template: `<<__EntryPoint>>
function main(): void {
  echo "Hello, world!\\n";
}
`,
  },
  haskell: {
    aliases: ["ghc", "ghci", "hs"],
    name: "Haskell",
    repl: "rm -f .ghci && ghci",
    main: "Main.hs",
    run: "(echo ':load Main' && echo 'main') > .ghci && ghci",
    format: "brittany Main.hs",
    lspSetup: "cp /opt/haskell/hie.yaml hie.yaml",
    lsp: "HIE_HOOGLE_DATABASE=/opt/haskell/hoogle.hoo hie --lsp",
    lspInit: {
      languageServerHaskell: {},
    },
    template: `module Main where

main :: IO ()
main = putStrLn "Hello, world!"
`,
  },
  haxe: {
    aliases: ["hx"],
    name: "Haxe",
    main: "Main.hx",
    compile: "haxe --main Main --js Main.js",
    run: "node Main.js",
    template: `class Main {
    static public function main() {
        trace("Hello, world!");
    }
}
`,
  },
  hcl: {
    aliases: ["tf", "terraform", "hashicorp", "hc"],
    name: "HCL",
    main: "main.hcl",
    compile: "cat main.hcl | yj -cj > main.json",
    run: "cat main.json | jq .",
    template: `output = "Hello, world!"
`,
  },
  hexagony: {
    aliases: ["hxg", "hex"],
    name: "Hexagony",
    main: "main.hxg",
    run: "/opt/hexagony/interpreter.rb main.hxg",
    template: `   H ; e ;
  l ; d ; *
 ; r ; o ; w
l ; ; o ; * 4
 3 3 ; @ . >
  ; 2 3 < \\
   4 ; * /
`,
  },
  hy: {
    name: "Hy",
    repl: "hy",
    main: "main.hy",
    run: "hy -i main.hy",
    template: `(print "Hello, world!")
`,
  },
  ink: {
    name: "Ink",
    repl: "ink",
    main: "main.ink",
    run: "ink main.ink; ink",
    template: `std := load('../../../opt/ink/std')
str := load('../../../opt/ink/str')

log := std.log

log('Hello, world!')
`,
  },
  intercal: {
    aliases: ["i", "ick"],
    name: "INTERCAL",
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
  ioke: {
    aliases: ["ik"],
    name: "Ioke",
    repl: "ioke",
    main: "main.ik",
    run: "ioke main.ik; ioke",
    template: `"Hello, world!" println
`,
  },
  java: {
    aliases: ["javac"],
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
    aliases: ["jl"],
    name: "Julia",
    repl: "julia",
    main: "main.jl",
    run: "julia -L main.jl",
    lsp: `JULIA_DEPOT_PATH=:/opt/julia julia -e 'using LanguageServer; run(LanguageServerInstance(stdin, stdout))'`,
    lspConfig: null,
    template: `println("Hello, world!")
`,
  },
  kalyn: {
    name: "Kalyn",
    main: "src-kalyn/Main.kalyn",
    compile: "kalyn",
    run: "out-kalyn/Main",
    template: `(import "/opt/kalyn/Stdlib.kalyn")

(public def main (IO Empty)
  (print "Hello, world!\\n"))
`,
  },
  kitten: {
    aliases: ["ktn"],
    name: "Kitten",
    repl: "kitten",
    main: "main.ktn",
    run: "kitten main.ktn; kitten",
    template: `"Hello, world!" say
`,
  },
  kotlin: {
    aliases: ["kts", "kotlinc"],
    name: "Kotlin",
    monacoLang: "kotlin",
    repl: "kotlinc",
    main: "main.kts",
    run: "kotlinc -script main.kts; kotlinc",
    template: `println("Hello, world!")
`,
  },
  ksh: {
    aliases: ["kshell"],
    name: "Ksh",
    monacoLang: "shell",
    repl: `SHELL=/usr/bin/ksh HOME="$PWD" ksh`,
    main: ".kshrc",
    createEmpty: ``,
    run: `SHELL=/usr/bin/ksh HOME="$PWD" ksh`,
    template: `echo "Hello, world!"
`,
  },
  less: {
    aliases: ["lessc"],
    name: "Less",
    monacoLang: "less",
    main: "main.less",
    run: "lessc main.less",
    format: "prettier --no-config main.less",
    template: `body:before {
  content: "Hello, world!";
}
`,
  },
  livescript: {
    aliases: ["lsc", "ls"],
    name: "LiveScript",
    repl: "lsc",
    main: "main.ls",
    run: "lsc -r ./main.ls; lsc",
    template: `console.log "Hello, world!"
`,
  },
  llvm: {
    name: "LLVM",
    monacoLang: "shell",
    main: "main.ll",
    compile: "clang -Wno-override-module main.ll -o main",
    run: "./main",
    template: `; Copied directly from the documentation
; Declare the string constant as a global constant.
@.str = private unnamed_addr constant [13 x i8] c"hello world\\0A\\00"

; External declaration of the puts function
declare i32 @puts(i8* nocapture) nounwind

; Definition of main function
define i32 @main() { ; i32()*
    ; Convert [13 x i8]* to i8  *...
    %cast210 = getelementptr [13 x i8],[13 x i8]* @.str, i64 0, i64 0

    ; Call puts function to write out the string to stdout.
    call i32 @puts(i8* %cast210)
    ret i32 0
}

; Named metadata
!0 = !{i32 42, null, !"string"}
!foo = !{!0}
`,
  },
  lolcode: {
    aliases: ["lol", "lci"],
    name: "LOLCODE",
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
    lsp: "java -cp /usr/lib/EmmyLua-LS.jar com.tang.vscode.MainKt",
    template: `print("Hello, world!")
`,
  },
  malbolge: {
    aliases: ["mb"],
    name: "Malbolge",
    main: "main.mb",
    run: "malbolge main.mb",
    template:
      " (=<`#9]~6ZY32Vx/4Rs+0No-&Jk)\"Fh}|Bcy?`=*z]Kw%oG4UUS0/@-ejc(:'8dc\n",
  },
  mariadb: {
    aliases: ["maria"],
    name: "MariaDB",
    repl: `rm -rf data && /opt/mariadb/scripts/mariadb-install-db --user="$(id -un)" && (/opt/mariadb/bin/mysqld --datadir="$PWD/data" --socket="$PWD/socket" --skip-networking &) && until [[ -e socket ]]; do sleep 0.01; done && mysql --socket="$PWD/socket"`,
    main: "main.sql",
    run: `rm -rf data && /opt/mariadb/scripts/mariadb-install-db --user="$(id -un)" && (/opt/mariadb/bin/mysqld --datadir="$PWD/data" --socket="$PWD/socket" --skip-networking &) && until [[ -e socket ]]; do sleep 0.01; done && (mysql --socket="$PWD/socket" < main.sql; mysql --socket="$PWD/socket")`,
    template: `SELECT 'Hello, world!'
`,
  },
  markdown: {
    aliases: [
      "mdown",
      "mkdn",
      "md",
      "mkd",
      "mdwn",
      "mdtxt",
      "mdtext",
      "text",
      "rmd",
    ],
    name: "Markdown",
    monacoLang: "markdown",
    main: "main.md",
    compile: "pandoc main.md -o main.html",
    run: "prettier --no-config main.html",
    format: "prettier --no-config main.md",
    template: `Hello, world!
`,
  },
  mediawiki: {
    aliases: ["media"],
    name: "MediaWiki",
    main: "main.txt",
    compile: "pandoc main.txt -f mediawiki -o main.html",
    run: "prettier --no-config main.html",
    template: `Hello, world!
`,
  },
  mips: {
    aliases: ["mips64"],
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
  mongodb: {
    aliases: ["mongo", "mongod"],
    name: "MongoDB",
    repl: `while ps -u "$(id -un)" -o comm | grep -q mongod; do sleep 0.01; done && rm -rf data && mkdir data && (mongod --dbpath=data --unixSocketPrefix="$PWD" --bind_ip=, &) && until mongo --host "$PWD/mongodb-27017.sock" --eval ' ' &>/dev/null; do sleep 0.01; done && mongo --host "$PWD/mongodb-27017.sock"`,
    main: "main.js",
    run: `while ps -u "$(id -un)" -o comm | grep -q mongod; do sleep 0.01; done && rm -rf data && mkdir data && (mongod --dbpath=data --unixSocketPrefix="$PWD" --bind_ip=, &) && until mongo --host "$PWD/mongodb-27017.sock" --eval ' ' &>/dev/null; do sleep 0.01; done && mongo --host "$PWD/mongodb-27017.sock" --shell main.js`,
    template: `print("Hello, world!")
`,
  },
  mumps: {
    aliases: ["mlang", "gtm", "fisgtm"],
    name: "MUMPS",
    main: "main.m",
    run:
      "gtm_dist=/usr/lib/x86_64-linux-gnu/fis-gtm/V6.3-007_x86_64 /usr/lib/x86_64-linux-gnu/fis-gtm/V6.3-007_x86_64/utf8/mumps -r main main.m",
    template: `main()
  write "Hello, world!",!
  quit
`,
  },
  mysql: {
    aliases: ["my"],
    name: "MySQL",
    repl: `rm -rf data && mysqld -h "$PWD/data" --initialize-insecure && (mysqld -h "$PWD/data" --socket="$PWD/socket" --pid-file="$PWD/pid-file" --mysqlx=OFF --skip-networking &) && until [[ -e socket ]]; do sleep 0.01; done && mysql --socket="$PWD/socket" -u root`,
    main: "main.sql",
    run: `rm -rf data && mysqld -h "$PWD/data" --initialize-insecure && (mysqld -h "$PWD/data" --socket="$PWD/socket" --pid-file="$PWD/pid-file" --mysqlx=OFF --skip-networking &) && until [[ -e socket ]]; do sleep 0.01; done && (mysql --socket="$PWD/socket" -u root < main.sql; mysql --socket="$PWD/socket" -u root)`,
    template: `SELECT 'Hello, world!'
`,
  },
  nim: {
    name: "Nim",
    main: "main.nim",
    compile: "nim compile main.nim",
    run: "./main",
    template: `echo "Hello, world!"
`,
  },
  nodejs: {
    aliases: ["node", "js", "javascript", "web", "jsx", "v8", "closure"],
    name: "Node.js",
    monacoLang: "javascript",
    repl: "node",
    main: "main.js",
    run: `node -e '
eval.apply(this, [require("fs").readFileSync("main.js", {encoding: "utf-8"})])
require("repl").start()
'`,
    format: "prettier --no-config main.js",
    pkg: {
      install: "yarn add NAME",
      uninstall: "yarn remove NAME",
      search:
        "curl -sS 'https://registry.npmjs.org/-/v1/search?text=NAME' | jq -r '.objects | map(.package.name) | .[]'",
    },
    template: `console.log("Hello, world!")
`,
  },
  objectivec: {
    aliases: ["objc", "gnustep"],
    name: "Objective-C",
    monacoLang: "objective-c",
    main: "main.m",
    compile:
      "gcc $(gnustep-config --objc-flags) main.m $(gnustep-config --base-libs) -o main",
    run: "./main",
    lspSetup: `(gnustep-config --objc-flags && gnustep-config --base-libs) | sed -E 's/\\s+/\\n/g' > compile_flags.txt`,
    lsp: "clangd",
    template: `#import <Foundation/Foundation.h>

int main() {
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  NSLog(@"Hello, world!");
  [pool drain];
  return 0;
}
`,
  },
  ocaml: {
    name: "OCaml",
    main: "main.ml",
    repl: "ocaml",
    run: "ocaml -init main.ml",
    format: "ocamlformat main.ml",
    lsp: "ocamllsp",
    lspLang: "ocaml",
    template: `print_string "Hello, world!\\n";;
`,
  },
  octave: {
    aliases: ["matlab", "m", "mathworks"],
    name: "Octave",
    repl: "octave",
    main: "main.m",
    run: "octave --persist main.m",
    template: `disp("Hello, world!")
`,
  },
  omgrofl: {
    aliases: ["omg", "rofl"],
    name: "Omgrofl",
    main: "main.omgrofl",
    run: "java -jar /opt/omgrofl/Omgrofl.jar main.omgrofl",
    template: `lol iz 72
rofl lol
lol iz 101
rofl lol
lol iz 108
rofl lol
rofl lol
lool iz 111
rofl lool
loool iz 44
rofl loool
loool iz 32
rofl loool
loool iz 119
rofl loool
rofl lool
lool iz 114
rofl lool
rofl lol
lol iz 100
rofl lol
lol iz 33
rofl lol
lol iz 10
rofl lol
`,
  },
  org: {
    aliases: ["orgmode"],
    name: "Org",
    main: "main.org",
    compile: "pandoc main.org -o main.html",
    run: "prettier --no-config main.html",
    template: `Hello, world!
`,
  },
  pascal: {
    aliases: ["pas", "fpc"],
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
    aliases: ["pl", "repl"],
    name: "Perl",
    monacoLang: "perl",
    repl: "re.pl",
    main: "main.pl",
    run: "re.pl --rcfile ./main.pl",
    template: `print("Hello, world!\\n")
`,
  },
  php: {
    aliases: ["phpcli"],
    name: "PHP",
    monacoLang: "php",
    repl: "php -a",
    main: "main.php",
    run: "php -d auto_prepend_file=main.php -a",
    lsp: "intelephense --stdio",
    template: `<?php

echo "Hello, world!\\n";
`,
  },
  pikachu: {
    aliases: [
      "pika",
      "pi",
      "ka",
      "chu",
      "pipi",
      "pichu",
      "pikapi",
      "poke",
      "pokemon",
      "pokeball",
    ],
    name: "Pikachu",
    main: "main.pokeball",
    run: "pikalang main.pokeball",
    template: `pi pi pi pi pi pi pi pi pi pi pika pipi pi pi pi pi pi pi pi pipi pi pi
pi pi pi pi pi pi pi pi pipi pi pi pi pipi pi pichu pichu pichu pichu ka
chu pipi pi pi pikachu pipi pi pikachu pi pi pi pi pi pi pi pikachu
pikachu pi pi pi pikachu pipi pi pi pikachu pichu pichu pi pi pi pi pi
pi pi pi pi pi pi pi pi pi pi pikachu pipi pikachu pi pi pi pikachu ka
ka ka ka ka ka pikachu ka ka ka ka ka ka ka ka pikachu pipi pi pikachu
pipi pikachu
`,
  },
  postgresql: {
    aliases: ["psql", "postgres", "pgsql", "postgre"],
    name: "PostgreSQL",
    monacoLang: "pgsql",
    repl: `rm -rf data && /usr/lib/postgresql/*/bin/initdb -D data && (echo "listen_addresses = ''" && echo "unix_socket_directories = '.'") >> data/postgresql.conf && /usr/lib/postgresql/*/bin/pg_ctl -D data -w start && psql -h "$PWD/data" postgres`,
    main: "main.sql",
    run: `rm -rf data && /usr/lib/postgresql/*/bin/initdb -D data && (echo "listen_addresses = ''" && echo "unix_socket_directories = '.'") >> data/postgresql.conf && /usr/lib/postgresql/*/bin/pg_ctl -D data -w start && (psql -h "$PWD/data" postgres -f main.sql; psql -h "$PWD/data" postgres)`,
    template: `SELECT 'Hello, world!';
`,
  },
  powershell: {
    aliases: ["pwsh", "ps1"],
    name: "PowerShell",
    monacoLang: "powershell",
    repl: "SHELL=/usr/bin/pwsh pwsh",
    main: "main.ps1",
    run: "SHELL=/usr/bin/pwsh pwsh -NoExit main.ps1",
    lsp: `pwsh -NoLogo -NoProfile -Command "/opt/powershell-editor-services/PowerShellEditorServices/Start-EditorServices.ps1 -BundledModulesPath /opt/powershell-editor-services -LogPath '$PWD/.powershell-editor-services/lsp.log' -SessionDetailsPath '$PWD/.powershell-editor-services/session.json' -FeatureFlags @() -AdditionalModules @() -HostName Riju -HostProfileId 'riju' -HostVersion 0.0 -Stdio -LogLevel Normal"`,
    template: `Write-Host "Hello, world!"
`,
  },
  prolog: {
    name: "Prolog",
    repl: "prolog",
    main: "main.pl",
    run: "prolog main.pl",
    template: `:- initialization main.

main :-
    write("Hello, world!"), nl.
`,
  },
  pug: {
    name: "Pug",
    monacoLang: "pug",
    main: "main.pug",
    compile: "pug main.pug",
    run: "prettier --no-config main.html",
    template: `html
  body
    p Hello, world!
`,
  },
  purescript: {
    aliases: ["purs", "pure", "ps"],
    name: "PureScript",
    setup: `shopt -s dotglob; cp -R /opt/purescript/project-template/* "$PWD/"`,
    repl: "spago repl",
    main: "src/Main.purs",
    run: `if spago build -n; then spago run -n; (echo 'import Prelude'; echo 'import Main') > .purs-repl; spago repl; else echo 'import Prelude' > .purs-repl; spago repl -d; fi`,
    template: `module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "Hello, world!"
`,
  },
  python: {
    aliases: ["python3", "python2", "py"],
    name: "Python",
    monacoLang: "python",
    repl: "python3 -u",
    main: "main.py",
    run: "python3 -u -i main.py",
    format: "cat main.py | black -",
    pkg: {
      install: "pip3 install --user NAME",
      uninstall: "pip3 uninstall NAME",
      search: `python3 -c 'import json; from xmlrpc import client; print(json.dumps(client.ServerProxy("https://pypi.org/pypi").search({"name": "NAME"})))' | jq -r 'map(.name) | .[]'`,
    },
    lsp: "Microsoft.Python.LanguageServer",
    lspInit: {
      interpreter: {
        properties: {
          InterpreterPath: "/usr/bin/python3",
        },
      },
    },
    template: `print('Hello, world!')
`,
  },
  قلب: {
    aliases: ["qalb"],
    name: "قلب",
    repl: "node /opt/qalb/repl.js",
    main: "main.qalb",
    run: "node /opt/qalb/repl.js main.qalb",
    template: `(قول "مرحبا يا عالم")
`,
  },
  r: {
    aliases: ["rlang"],
    name: "R",
    monacoLang: "r",
    repl: "R",
    main: ".Rprofile",
    run: "R --no-save",
    template: `print("Hello, world!")
`,
  },
  racket: {
    aliases: ["rkt"],
    name: "Racket",
    repl: "racket",
    main: "main.rkt",
    run: `racket -i -e '(enter! "main.rkt") (display "[ type (enter! \\"main.rkt\\") to access local variables ]\\n")'`,
    template: `#lang racket/base
(display "Hello, world!\\n")
`,
  },
  рапира: {
    aliases: ["rap", "rerap", "rerap2", "rapira"],
    name: "Рапира",
    main: "main.rap",
    run: "rapira main.rap",
    template: `вывод: "Hello, world!"
`,
  },
  reasonml: {
    aliases: ["re", "reason", "bsc", "buckle", "bucklescript"],
    name: "ReasonML",
    main: "main.re",
    compile: "bsc main.re > main.js",
    run: "NODE_PATH=/usr/lib/node_modules node main.js",
    format: "ocamlformat main.re",
    lspSetup: `cp -a /opt/reasonml/project-template/* ./`,
    lsp: "reason-language-server",
    template: `print_string("Hello, world!\\n")
`,
  },
  redis: {
    name: "Redis",
    monacoLang: "redis",
    repl:
      "rm -f socket; (redis-server --port 0 --unixsocket socket &); until [[ -e socket ]]; do sleep 0.01; done; redis-cli -s socket",
    main: "main.redis",
    run:
      "rm -f socket; (redis-server --port 0 --unixsocket socket &); until [[ -e socket ]]; do sleep 0.01; done; redis-cli -s socket < main.redis; redis-cli -s socket",
    template: `ECHO "Hello, world!"
`,
  },
  restructuredtext: {
    aliases: ["rst"],
    name: "reStructuredText",
    monacoLang: "restructuredtext",
    main: "main.rst",
    compile: "pandoc main.rst -o main.html",
    run: "prettier --no-config main.html",
    template: `Hello, world!
`,
  },
  riscv: {
    aliases: ["risc"],
    name: "RISC-V",
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
  roff: {
    aliases: [
      "groff",
      "nroff",
      "troff",
      "1",
      "2",
      "3",
      "4",
      "5",
      "6",
      "7",
      "8",
      "9",
      "man",
      "manual",
    ],
    name: "roff",
    main: "main.roff",
    compile: "pandoc main.roff -f man -o main.html",
    run: "prettier --no-config main.html",
    template: `.PP
Hello, world!
`,
  },
  ruby: {
    aliases: ["irb", "rb"],
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
    pkg: {
      install: "gem install --user-install NAME",
      uninstall: "gem uninstall --user-install NAME",
      search: `curl -sS 'https://rubygems.org/api/v1/search.json?query=NAME' | jq -r 'map(.name) | .[]'`,
    },
    lsp: "solargraph stdio",
    template: `puts "Hello, world!"
`,
    test: {
      ensure: `ruby -e 'raise "version mismatch, expected #{RUBY_VERSION}" unless ENV["PATH"].include? ".gem/ruby/#{RUBY_VERSION}/bin"'`,
    },
  },
  rust: {
    aliases: ["rs", "rustc"],
    name: "Rust",
    monacoLang: "rust",
    main: "main.rs",
    compile: "rustc main.rs",
    run: "./main",
    lsp: "rls",
    template: `fn main() {
    println!("Hello, world!");
}
`,
  },
  sass: {
    name: "Sass",
    main: "main.sass",
    run: "sass main.sass",
    template: `body:before
  content: "Hello, world!"
`,
  },
  scala: {
    name: "Scala",
    repl: "scala",
    main: "main.scala",
    run: "scala -i main.scala",
    template: `println("Hello, world!")
`,
  },
  scheme: {
    aliases: ["scm", "mitscheme"],
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
  scss: {
    name: "SCSS",
    monacoLang: "scss",
    main: "main.scss",
    run: "sass main.scss",
    format: "prettier --no-config main.scss",
    template: `body:before {
  content: "Hello, world!";
}
`,
  },
  sed: {
    aliases: ["gsed"],
    name: "Sed",
    main: "main.sed",
    run: "echo 'Reading from stdin...' >&2; sed -f main.sed",
    template: `s/.*/Hello, world!/
`,
  },
  setl: {
    name: "SETL",
    main: "main.setl",
    run: "setl main.setl",
    template: `print("Hello, world!");
`,
  },
  sh: {
    aliases: ["shell", "posix", "posixsh", "ash", "dash", "posh"],
    name: "Sh",
    monacoLang: "shell",
    repl: `SHELL=/usr/bin/sh HOME="$PWD" posh -l`,
    main: ".profile",
    createEmpty: ``,
    run: `SHELL=/usr/bin/sh HOME="$PWD" posh -l`,
    template: `echo "Hello, world!"
`,
  },
  shakespeare: {
    aliases: ["spl"],
    name: "Shakespeare",
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
    aliases: ["gst", "st"],
    name: "Smalltalk",
    repl: "gst",
    main: "main.st",
    run: "gst main.st; gst",
    template: `'Hello, world!' displayNl !
`,
  },
  snobol: {
    aliases: ["snobol4", "spitbol", "sno"],
    name: "SNOBOL",
    repl: "snobol4",
    main: "main.sno",
    run: "snobol4 main.sno; snobol4",
    template: ` OUTPUT = "Hello, world!"
END
`,
  },
  sqlite: {
    aliases: ["sql", "db", "sqlite3"],
    name: "SQLite",
    monacoLang: "sql",
    repl: "sqlite3",
    main: "main.sql",
    run: `sqlite3 -cmd "$(< main.sql)"`,
    template: `SELECT 'Hello, world!';
`,
  },
  standardml: {
    aliases: ["sml", "ml"],
    name: "Standard ML",
    repl: "rlwrap sml",
    main: "main.sml",
    run: "rlwrap sml main.sml",
    template: `print "Hello, world!\\n";
`,
  },
  swift: {
    aliases: ["swiftc"],
    name: "Swift",
    monacoLang: "swift",
    main: "main.swift",
    compile: "swiftc main.swift",
    run: "./main",
    lsp: "sourcekit-lsp",
    template: `print("Hello, world!")
`,
  },
  tcl: {
    aliases: ["tclsh", "tclshrc"],
    name: "Tcl",
    monacoLang: "tcl",
    repl: "tclsh",
    main: ".tclshrc",
    createEmpty: ``,
    run: `HOME="$PWD" tclsh`,
    template: `puts {Hello, world!}
`,
  },
  tcsh: {
    aliases: ["tcshell", "tcshrc"],
    name: "Tcsh",
    monacoLang: "shell",
    repl: `SHELL=/usr/bin/tcsh HOME="$PWD" tcsh`,
    main: ".tcshrc",
    createEmpty: ``,
    run: `SHELL=/usr/bin/tcsh HOME="$PWD" tcsh`,
    template: `echo "Hello, world!"
`,
  },
  tex: {
    aliases: ["latex", "xetex", "plaintex"],
    name: "TeX",
    repl: "tex",
    main: "main.tex",
    run: "tex main.tex",
    lsp: "digestif",
    lspLang: "tex",
    template: `\\message{Hello, world!}
`,
  },
  textile: {
    name: "Textile",
    main: "main.textile",
    compile: "pandoc main.textile -o main.html",
    run: "prettier --no-config main.html",
    template: `Hello, world!
`,
  },
  thue: {
    name: "Thue",
    main: "main.thue",
    run: "thue main.thue",
    template: `a::=~Hello, world!
::=
a
`,
  },
  tikiwiki: {
    aliases: ["tiki"],
    name: "Tiki Wiki",
    main: "main.txt",
    compile: "pandoc main.txt -f tikiwiki -o main.html",
    run: "prettier --no-config main.html",
    template: `Hello, world!
`,
  },
  toml: {
    aliases: ["tom"],
    name: "TOML",
    main: "main.toml",
    compile: "cat main.toml | yj -tj > main.json",
    run: "cat main.json | jq .",
    template: `output = "Hello, world!"
`,
  },
  twiki: {
    name: "TWiki",
    main: "main.txt",
    compile: "pandoc main.txt -f twiki -o main.html",
    run: "prettier --no-config main.html",
    template: `Hello, world!
`,
  },
  typescript: {
    aliases: ["ts", "tsnode", "tsc"],
    name: "TypeScript",
    monacoLang: "typescript",
    repl: "ts-node",
    main: "main.ts",
    run: `ts-node -i -e "$(< main.ts)"`,
    format: "prettier --no-config main.ts",
    template: `console.log("Hello, world!");
`,
  },
  unlambda: {
    aliases: ["unl"],
    name: "Unlambda",
    repl: "unlambda-repl",
    main: "main.unl",
    run: "unlambda-repl main.unl",
    template: "`.\n`.!`.d`.l`.r`.o`.w`. `.,`.o`.l`.l`.e`.Hi\n",
  },
  vim: {
    aliases: ["viml", "vimscript"],
    name: "Vimscript",
    repl: "vim",
    main: "main.vim",
    run: `vim -c "$(< main.vim)"`,
    lsp: "vim-language-server --stdio",
    template: `:echo "Hello, world!"
`,
  },
  vimwiki: {
    name: "Vimwiki",
    main: "main.txt",
    compile: "pandoc main.txt -f vimwiki -o main.html",
    run: "prettier --no-config main.html",
    template: `Hello, world!
`,
  },
  visualbasic: {
    aliases: ["vbasic", "vb", "vbnc"],
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
    aliases: ["ws"],
    name: "Whitespace",
    main: "main.ws",
    run: "whitespace main.ws",
    template: `Hello, world!  \t  \t   \n\t\n     \t\t  \t \t\n\t\n     \t\t \t\t  \n\t\n     \t\t \t\t  \n\t\n     \t\t \t\t\t\t\n\t\n     \t \t\t  \n\t\n     \t     \n\t\n     \t\t\t \t\t\t\n\t\n     \t\t \t\t\t\t\n\t\n     \t\t\t  \t \n\t\n     \t\t \t\t  \n\t\n     \t\t  \t  \n\t\n  \n\n\n`,
  },
  wolframlanguage: {
    aliases: [
      "wolfram",
      "mathematica",
      "mathics",
      "wolframmathematica",
      "wls",
      "expreduce",
      "symja",
    ],
    name: "Wolfram Language",
    repl: "mathics",
    main: "main.wls",
    run: "mathics --persist main.wls",
    template: `Print["Hello, world!"]
`,
  },
  x86: {
    aliases: ["s", "asm", "assembly", "x86-64"],
    name: "x86",
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
  yaml: {
    aliases: ["yml"],
    name: "YAML",
    monacoLang: "yaml",
    main: "main.yaml",
    compile: "cat main.yaml | yj -yj > main.json",
    run: "cat main.json | jq .",
    format: "prettier --no-config main.yaml",
    template: `output: "Hello, world!"
`,
  },
  zot: {
    name: "Zot",
    main: "main.zot",
    run: "zot --file main.zot",
    template: `111101010100111010101001001101010010010011101010100111010101
001101010010101010011101010100110101001101010100110101001010
101001110101010011101010100110101001010101001110101010011010
100110101010011010100101010100111010101001101010011010101001
101010011010101001110101010011101010100111010101001110101010
010011010100100110101001001101010010011010100101010011101010
100110101001101010100110101001101010100110101001010100111010
101001110101010011010100101010100111010101001101010011010101
001101010010101010011101010100110101001101010100111010101001
101010010101010010101001110101010011010100101010011101010100
111010101001101010010101010011101010100110101001010100111010
101001101010010101010010101001101010011101010100110101001101
010100100101010011010100101010011101010100110101001101010100
110101001101010100110101001010100111010101001110101010011010
100101010100111010101001101010011010101001101010010101010011
101010100110101001101010100111010101001101010010101010010101
001110101010011010100101010011101010100111010101001101010010
101010011101010100110101001010100111010101001101010010101010
010101001101010011101010100110101001101010100100101010011010
100101010011101010100110101001010100101010001010000100001000
010011000110110010011101111011011101110000001000011010011110
11000110110001101101010011000010010
`,
  },
  zsh: {
    aliases: ["zshell", "zshrc"],
    name: "Zsh",
    monacoLang: "shell",
    repl: "SHELL=/usr/bin/zsh zsh",
    main: ".zshrc",
    createEmpty: ``,
    run: `SHELL=/usr/bin/zsh ZDOTDIR="$PWD" zsh`,
    template: `echo "Hello, world!"
`,
  },
};
