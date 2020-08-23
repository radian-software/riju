export interface LangConfig {
  aliases?: string[];
  name: string;
  monacoLang?: string;
  daemon?: string;
  setup?: string;
  repl?: string;
  input?: string;
  output?: string;
  main: string;
  prefix?: string;
  suffix?: string;
  createEmpty?: string;
  compile?: string;
  run: string;
  helloInput?: string;
  hello?: string;
  helloMaxLength?: number;
  runReplInput?: string;
  runReplOutput?: string;
  scope?: {
    code: string;
    after?: string;
    input?: string;
    output?: string;
  };
  ensure?: string;
  format?: {
    run: string;
    input?: string;
    output?: string;
  };
  pkg?: {
    install: string;
    uninstall?: string;
    all?: string;
    search?: string;
  };
  lsp?: {
    setup?: string;
    start: string;
    disableDynamicRegistration?: boolean;
    init?: any;
    config?: any;
    lang?: string;
    code?: string; // required unless test is skipped
    after?: string;
    item?: string; // required unless test is skipped
  };
  template: string;
  timeout?: number;
  skip?: string[];
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
    lsp: {
      start: "ada_language_server",
      code: `
   Ada.IO`,
      after: `);`,
      item: "IO_Exceptions",
    },
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
    input: "123 × 234",
    main: "main.apl",
    run: "apl -f main.apl",
    scope: {
      code: `x ← 123 × 234`,
    },
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
    input: `expr 123 \\* 234`,
    main: "main.bash",
    run: "bash --rcfile main.bash",
    scope: { code: `x="$(expr 123 \\* 234)"`, input: `echo "$x"` },
    lsp: {
      start: "bash-language-server start",
      code: "read",
      item: "readonly",
    },
    template: `echo "Hello, world!"
`,
  },
  basic: {
    aliases: ["bas", "qbasic"],
    name: "BASIC",
    repl: "bwbasic",
    input: "PRINT 123 * 234",
    main: "main.bas",
    run: "bwbasic main.bas",
    scope: {
      code: `x = 123 * 234`,
      input: `PRINT x`,
    },
    template: `PRINT "Hello, world!"
`,
  },
  battlestar: {
    aliases: ["battlestarc", "bts"],
    name: "Battlestar",
    main: "main.bts",
    run: "bts main.bts",
    template: `const message = "Hello, world!\n"

fun main
    syscall(1, 1, message, len(message))
end
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
  boo: {
    aliases: ["booc"],
    name: "Boo",
    setup: `mkdir -p "$HOME/.local/share" && touch "$HOME/.local/share/booish_history"`,
    main: "main.boo",
    repl: "booish",
    compile: "booc main.boo",
    run: "mono main.exe; booish",
    template: `print "Hello, world!"
`,
  },
  brainf: {
    aliases: ["brainfuck", "bf"],
    name: "Brainf***",
    repl: "brainf-repl",
    input:
      ">++>+[>++++[-<++++>]<<]> [>>+>+<<<-]>>>[<<<+>>>-]<<+>[<->[>++++++++++<[->-[>+>>]>[+[-<+>]>+>>]<<<<<]>[-]++++++++[<++++++>-]>[<<+>>-]>[<<+>>-]<<]>]<[->>++++++++[<++++++>-]]<[.[-]<]<",
    output: "86",
    main: "main.bf",
    run: "brainf-repl main.bf",
    hello: "Hello World",
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
    format: {
      run: "clang-format --assume-filename=format.c",
      input: `#include <stdio.h>

int main()
{
  printf("Hello, world!\\n");
  return 0;
}
`,
    },
    lsp: {
      setup: `echo '-Wall -Wextra' | sed -E 's/\\s+/\\n/g' > compile_flags.txt`,
      start: "clangd",
    },
    template: `#include <stdio.h>

int main() {
  printf("Hello, world!\\n");
  return 0;
}
`,
    skip: ["lsp"],
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
    format: {
      run: "clang-format --assume-filename=format.cpp",
      input: `#include <iostream>

int main()
{
  std::cout << "Hello, world!" << std::endl;
  return 0;
}
`,
    },
    lsp: {
      setup: `echo '-Wall -Wextra' | sed -E 's/\\s+/\\n/g' > compile_flags.txt`,
      start: "clangd",
    },
    template: `#include <iostream>

int main() {
  std::cout << "Hello, world!" << std::endl;
  return 0;
}
`,
    skip: ["lsp"],
  },
  cat: {
    aliases: ["cat-language"],
    name: "Cat",
    repl: "NODE_PATH=/opt/cat node /opt/cat/repl.js",
    input: "123 234 mul",
    main: "main.cat",
    run: "NODE_PATH=/opt/cat node /opt/cat/repl.js main.cat",
    hello: "72,101,108,108,111,44,32,119,111,114,108,100,33,10",
    template: `72 101 108 108 111 44 32 119 111 114 108 100 33 10
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
    timeout: 15,
  },
  chef: {
    name: "Chef",
    main: "main.chef",
    run: "chef main.chef",
    hello: "Hello world!",
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
  clean: {
    aliases: ["icl", "clm"],
    name: "Clean",
    main: "main.icl",
    compile: "clm main -o main",
    run: "./main",
    template: `module main

import StdEnv

Start world
  #(console, world) = stdio world
  #console = fwrites "Hello, world!\\n" console
  #(ok, world) = fclose console world
  = world
`,
  },
  clojure: {
    aliases: ["clj"],
    name: "Clojure",
    monacoLang: "clojure",
    repl: "clojure",
    input: "(* 123 234)",
    main: "main.clj",
    run: "clojure -i main.clj -r",
    scope: {
      code: `(def x (* 123 234))`,
    },
    lsp: { start: "clojure-lsp" },
    template: `(println "Hello, world!")
`,
    skip: ["lsp"],
  },
  clojurescript: {
    aliases: ["cljs", "lumo"],
    name: "ClojureScript",
    monacoLang: "clojure",
    repl: "lumo -r",
    input: "(* 123 234)",
    main: "main.cljs",
    run: "lumo -i main.cljs -r",
    scope: {
      code: `(def x (* 123 234))`,
    },
    template: `(println "Hello, world!")
`,
  },
  cmd: {
    aliases: ["bat", "batch", "wine"],
    name: "Cmd",
    monacoLang: "bat",
    setup: "shopt -s dotglob; cp -R /opt/cmd/home-template/* ./",
    repl: "wine cmd",
    input: "set /a 123 * 234",
    main: "main.bat",
    run: `wine cmd /k main.bat`,
    scope: {
      code: `set /a x = 123 * 234`,
      input: `echo %x%`,
    },
    template: `echo "Hello, world!"
`,
    timeout: 15,
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
    scope: {
      code: `x = 123 * 234`,
    },
    template: `console.log "Hello, world!"
`,
  },
  commonlisp: {
    aliases: ["lisp", "sbcl"],
    name: "Common Lisp",
    repl: "rlwrap sbcl",
    input: "(* 123 234)",
    main: "main.lisp",
    run: "rlwrap sbcl --userinit main.lisp",
    scope: {
      code: `(defvar x (* 123 234))`,
    },
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
  crystal: {
    aliases: ["cr"],
    name: "Crystal",
    main: "main.cr",
    run: "crystal main.cr",
    template: `puts "Hello, world!"
`,
    timeout: 15,
  },
  csharp: {
    aliases: ["cs", "mcs"],
    name: "C#",
    monacoLang: "csharp",
    main: "main.cs",
    compile: "mcs main.cs",
    run: "mono main.exe",
    format: {
      run: `clang-format --style="{BasedOnStyle: llvm, IndentWidth: 4}" --assume-filename=format.cs`,
      input: `class main
{
    static void Main(string[] args)
    {
        System.Console.WriteLine("Hello, world!");
    }
}
`,
    },
    template: `class main {
    static void Main(string[] args) {
        System.Console.WriteLine("Hello, world!");
    }
}
`,
  },
  d: {
    aliases: ["dmd"],
    name: "D",
    main: "main.d",
    compile: "dmd main.d",
    run: "./main",
    format: {
      run: "dfmt",
      input: `import std.stdio;

void main() {
    writeln("Hello, world!");
}
`,
    },
    template: `import std.stdio;

void main()
{
    writeln("Hello, world!");
}
`,
  },
  dart: {
    name: "Dart",
    monacoLang: "dart",
    main: "main.dart",
    run: "dart main.dart",
    lsp: {
      start:
        "dart /usr/lib/dart/bin/snapshots/analysis_server.dart.snapshot --lsp",
      disableDynamicRegistration: true,
      lang: "dart",
    },
    template: `void main() {
  print('Hello, world!');
}
`,
    skip: ["lsp"],
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
  dylan: {
    aliases: ["opendylan"],
    name: "Dylan",
    setup: "cp -R /opt/dylan/project-template/* ./",
    main: "main.dylan",
    compile: "dylan-compiler -build main.lid",
    run: "_build/bin/main",
    template: `Module: main

define function main
    (name :: <string>, arguments :: <vector>)
  format-out("Hello, world!\\n");
  exit-application(0);
end function main;

main(application-name(), application-arguments());
`,
  },
  elixir: {
    aliases: ["iex", "exs"],
    name: "Elixir",
    repl: "iex",
    main: "main.exs",
    run: "iex main.exs",
    scope: {
      code: `defmodule Scope do
  def x do
    123 * 234
  end
end`,
      input: `Scope.x`,
    },
    lsp: { start: "/opt/elixir-ls/language_server.sh" },
    template: `IO.puts("Hello, world!")
`,
    skip: ["repl", "runrepl", "scope", "lsp"],
  },
  elm: {
    name: "Elm",
    repl: "elm repl",
    main: "Main.elm",
    run: "cp /opt/elm/elm.json elm.json && run-elm Main.elm; elm repl",
    scope: {
      code: `x = 123 * 234`,
      input: `import Main
Main.x`,
    },
    lsp: {
      setup: "cp /opt/elm/elm.json elm.json",
      start: "elm-language-server --stdio",
    },
    template: `module Main exposing (..)

output : String
output = "Hello, world!"
`,
    skip: ["lsp"],
  },
  elvish: {
    aliases: ["elv"],
    name: "Elvish",
    repl: `SHELL=/usr/bin/elvish HOME="$PWD" elvish`,
    input: `* 123 234`,
    main: ".elvish/rc.elv",
    createEmpty: ``,
    run: `SHELL=/usr/bin/elvish HOME="$PWD" elvish`,
    scope: {
      code: `x = (* 123 234)`,
      input: `echo $x`,
    },
    template: `echo "Hello, world!"
`,
  },
  emacs: {
    aliases: ["emacslisp", "elisp", "gnuemacs", "xemacs", "ielm"],
    name: "Emacs Lisp",
    repl: `emacs --eval "(progn (require 'package) (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) (package-initialize) (ielm))"`,
    input: "(* 123 234)",
    main: "main.el",
    run: `emacs --load main.el --eval "(progn (require 'package) (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) (package-initialize) (ielm))"`,
    scope: {
      code: `(defvar x (* 123 234))`,
    },
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
    hello: `[F-J][c-g][j-n][j-n][m-q][*-.][\\x1e-"][u-y][m-q][p-t][j-n][b-f][\\x1f-#]`,
    helloMaxLength: "Hello, world!".length,
    template: `Program MyNamespace MyProgram [
	print "Hello, world!";
]
`,
  },
  erlang: {
    aliases: ["erl"],
    name: "Erlang",
    repl: "erl",
    input: "123 * 234.",
    main: "main.erl",
    compile: "erl -compile main",
    run: "erl -s main main",
    lsp: { start: "erlang_ls" },
    scope: {
      code: `-export([x/0]).
x() -> 123 * 234.
`,
      after: `-export([main/0]).`,
      input: `main:x().`,
    },
    template: `-module(main).
-export([main/0]).

main() ->
    io:fwrite("Hello, world!\\n").
`,
    skip: ["lsp"],
  },
  euphoria: {
    aliases: ["ex", "exw", "exu", "euc", "eui", "eub"],
    name: "Euphoria",
    main: "main.exu",
    run: "exu main.exu",
    template: `puts(1, "Hello, world!\\n")
`,
  },
  எழில்: {
    aliases: ["ezhil", "ezhili", "ezhuthi", "tamil"],
    name: "எழில்",
    repl: "ezhili",
    main: "main.n",
    run: "ezhili main.n; ezhili",
    hello: "வணக்கம், உலகமே!",
    template: `பதிப்பி "வணக்கம், உலகமே!"
`,
  },
  factor: {
    aliases: ["fact"],
    name: "Factor",
    repl: "factor-lang",
    input: "123 234 *",
    main: ".factor-rc",
    createEmpty: ``,
    run: "factor-lang",
    scope: {
      code: `USE: math
: x ( -- x ) 123 234 * ;`,
      input: `USE: main
x`,
    },
    template: `IN: main
USE: io

"Hello, world!" print
`,
  },
  fish: {
    name: "Fish",
    repl: "SHELL=/usr/bin/fish fish",
    input: `expr 123 \\* 234`,
    main: "main.fish",
    run: 'SHELL=/usr/bin/fish fish -C "$(< main.fish)"',
    scope: {
      code: `set x (expr 123 \\* 234)`,
      input: `echo $x`,
    },
    template: `echo "Hello, world!"
`,
  },
  forth: {
    aliases: ["gforth"],
    name: "Forth",
    repl: "gforth",
    input: "123 234 * .",
    main: "main.fs",
    run: "gforth main.fs",
    scope: {
      code: `VARIABLE X
123 234 * X !`,
      input: `X @ .`,
    },
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
    lsp: { start: "fortls" },
    template: `       program hello
          print *, "Hello, world!"
       end program hello
`,
    skip: ["lsp"],
  },
  fsharp: {
    aliases: ["fsharpi", "fsx", "fs"],
    name: "F#",
    monacoLang: "fsharp",
    repl: "fsharpi",
    input: "123 * 234 ;;",
    main: "main.fsx",
    run: "fsharpi --use:main.fsx",
    scope: {
      code: `let x = 123 * 234`,
      input: `x ;;`,
    },
    template: `printfn "Hello, world!"
`,
    timeout: 15,
  },
  go: {
    aliases: ["golang"],
    name: "Go",
    monacoLang: "go",
    main: "main.go",
    compile: "go build main.go",
    run: "./main",
    format: {
      run: "gofmt",
      input: `package main

import "fmt"

func main() {
	fmt.Println("Hello, world!");
}
`,
    },
    lsp: { start: "gopls" },
    template: `package main

import "fmt"

func main() {
	fmt.Println("Hello, world!")
}
`,
    skip: ["lsp"],
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
    scope: {
      code: `x = 123 * 234;`,
    },
    template: `print "Hello, world!";
`,
    timeout: 15,
  },
  hack: {
    aliases: ["hhvm"],
    name: "Hack",
    repl: "hhvm -a",
    input: "print 123 * 234",
    main: "main.hack",
    run: `echo "Type 'r' at the debugger prompt to run the code" && hhvm -a main.hack`,
    helloInput: "r",
    scope: {
      code: `function x() : int {
  return 123 * 234;
}`,
      input: `r
p x()`,
    },
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
    scope: {
      code: `x = 123 * 234`,
    },
    format: {
      run: "brittany",
      input: `module Main where

main :: IO ()
main =
  putStrLn "Hello, world!"
`,
    },
    lsp: {
      setup: "cp /opt/haskell/hie.yaml hie.yaml",
      start: "HIE_HOOGLE_DATABASE=/opt/haskell/hoogle.hoo hie --lsp",
      init: {
        languageServerHaskell: {},
      },
    },
    template: `module Main where

main :: IO ()
main = putStrLn "Hello, world!"
`,
    skip: ["lsp"],
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
    input: "(* 123 234)",
    main: "main.hy",
    run: "hy -i main.hy",
    scope: {
      code: `(setv x (* 123 234))`,
    },
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
    compile: "ick -b main.i",
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
    repl: `JAVA_OPTS="-Duser.home=$PWD" ioke`,
    main: "main.ik",
    run: `JAVA_OPTS="-Duser.home=$PWD" ioke main.ik; JAVA_OPTS="-Duser.home=$PWD" ioke`,
    template: `"Hello, world!" println
`,
    timeout: 15,
  },
  j: {
    aliases: ["jconsole", "ijconsole"],
    name: "J",
    repl: "echo 'ijconsole:' && ijconsole",
    main: "main.ijs",
    run: "ijconsole main.ijs",
    template: `echo 'Hello, world!'
`,
  },
  java: {
    aliases: ["javac"],
    name: "Java",
    monacoLang: "java",
    main: "Main.java",
    compile: "javac Main.java",
    run: "java Main",
    format: {
      run: `clang-format --style="{BasedOnStyle: llvm, IndentWidth: 4}" --assume-filename=Format.java`,
      input: `public class Main
{
    public static void main(String[] args)
    {
        System.out.println("Hello, world!");
    }
}
`,
    },
    lsp: {
      setup: "rm -rf jdt && cp -RT /opt/jdt/config_linux jdt",
      start: `java -Declipse.application=org.eclipse.jdt.ls.core.id1 -Dosgi.bundles.defaultStartLevel=4 -Declipse.product=org.eclipse.jdt.ls.core.product -Dlog.level=ALL -noverify -Xmx1G -jar /opt/jdt/plugins/org.eclipse.equinox.launcher_*.jar -configuration "$PWD/jdt" -data "$PWD" --add-modules=ALL-SYSTEM --add-opens java.base/java.util=ALL-UNNAMED --add-opens java.base/java.lang=ALL-UNNAMED`,
      init: {
        settings: {
          java: {
            codeGeneration: {
              toString: {
                limitElements: 0,
                listArrayContents: true,
                skipNullValues: false,
                codeStyle: "STRING_CONCATENATION",
                template:
                  "${object.className} [${member.name()}=${member.value}, ${otherMembers}]",
              },
              generateComments: false,
              useBlocks: false,
              hashCodeEquals: {
                useInstanceof: false,
                useJava7Objects: false,
              },
            },
            format: {
              onType: {
                enabled: true,
              },
              comments: {
                enabled: true,
              },
              enabled: true,
            },
            progressReports: {
              enabled: true,
            },
            foldingRange: {
              enabled: true,
            },
            completion: {
              importOrder: ["java", "javax", "com", "org"],
              favoriteStaticMembers: [
                "org.junit.Assert.*",
                "org.junit.Assume.*",
                "org.junit.jupiter.api.Assertions.*",
                "org.junit.jupiter.api.Assumptions.*",
                "org.junit.jupiter.api.DynamicContainer.*",
                "org.junit.jupiter.api.DynamicTest.*",
                "org.mockito.Mockito.*",
                "org.mockito.ArgumentMatchers.*",
                "org.mockito.Answers.*",
              ],
              guessMethodArguments: true,
              overwrite: true,
              enabled: true,
              filteredTypes: ["java.awt.*", "com.sun.*"],
            },
            maxConcurrentBuilds: 1,
            autobuild: {
              enabled: true,
            },
            selection: {
              enabled: true,
            },
            import: {
              exclusions: [
                "**/node_modules/**",
                "**/.metadata/**",
                "**/archetype-resources/**",
                "**/META-INF/maven/**",
              ],
              maven: {
                enabled: true,
              },
              gradle: {
                enabled: true,
                wrapper: {
                  enabled: true,
                },
              },
            },
            saveActions: {
              organizeImports: false,
            },
            implementationsCodeLens: {
              enabled: false,
            },
            signatureHelp: {
              enabled: true,
            },
            referencesCodeLens: {
              enabled: false,
            },
            maven: {
              downloadSources: false,
            },
            trace: {
              server: "off",
            },
            configuration: {
              updateBuildConfiguration: "automatic",
              checkProjectSettingsExclusions: true,
            },
            errors: {
              incompleteClasspath: {
                severity: "warning",
              },
            },
            dependency: {
              packagePresentation: "flat",
            },
          },
        },
        extendedClientCapabilities: {
          progressReportProvider: true,
          classFileContentsSupport: true,
          overrideMethodsPromptSupport: true,
          hashCodeEqualsPromptSupport: true,
          advancedOrganizeImportsSupport: true,
          generateConstructorsPromptSupport: true,
          generateToStringPromptSupport: true,
          advancedGenerateAccessorsSupport: true,
          advancedExtractRefactoringSupport: true,
          moveRefactoringSupport: true,
        },
        bundles: [
          "/opt/jdt/bundles/com.microsoft.java.test.plugin-0.19.0.jar",
          "/opt/jdt/bundles/com.microsoft.jdtls.ext.core-0.5.1.jar",
          "/opt/jdt/bundles/dg.jdt.ls.decompiler.cfr-0.0.2-201802221740.jar",
          "/opt/jdt/bundles/dg.jdt.ls.decompiler.common-0.0.2-201802221740.jar",
          "/opt/jdt/bundles/dg.jdt.ls.decompiler.fernflower-0.0.2-201802221740.jar",
          "/opt/jdt/bundles/dg.jdt.ls.decompiler.procyon-0.0.2-201802221740.jar",
          "/opt/jdt/bundles/io.projectreactor.reactor-core.jar",
          "/opt/jdt/bundles/java.debug.plugin.jar",
          "/opt/jdt/bundles/jdt-ls-commons.jar",
          "/opt/jdt/bundles/jdt-ls-extension.jar",
          "/opt/jdt/bundles/org.reactivestreams.reactive-streams.jar",
        ],
      },
    },
    template: `public class Main {
    public static void main(String[] args) {
        System.out.println("Hello, world!");
    }
}
`,
    skip: ["lsp"],
  },
  javascript: {
    aliases: ["node", "js", "web", "jsx", "v8", "closure", "nodejs"],
    name: "Node.js",
    monacoLang: "javascript",
    repl: "node",
    main: "main.js",
    run: `node -e "$(< main.js)" -i`,
    scope: {
      code: `let x = 123 * 234;`,
    },
    format: {
      run: "prettier --no-config --stdin-filepath=format.js",
      input: `console.log('Hello, world!');
`,
    },
    pkg: {
      install: "yarn add NAME",
      uninstall: "yarn remove NAME",
      search:
        "curl -sS 'https://registry.npmjs.org/-/v1/search?text=NAME' | jq -r '.objects | map(.package.name) | .[]'",
    },
    template: `console.log("Hello, world!");
`,
  },
  julia: {
    aliases: ["jl"],
    name: "Julia",
    repl: "julia",
    main: "main.jl",
    run: "julia -L main.jl",
    scope: {
      code: `x = 123 * 234`,
    },
    lsp: {
      start: `JULIA_DEPOT_PATH=:/opt/julia julia -e 'using LanguageServer; run(LanguageServerInstance(stdin, stdout))'`,
      config: null,
    },
    template: `println("Hello, world!")
`,
    skip: ["lsp"],
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
    input: "(123 * 234) say",
    main: "main.ktn",
    run: "kitten main.ktn; kitten",
    template: `"Hello, world!" say
`,
    timeout: 15,
  },
  kotlin: {
    aliases: ["kts", "kotlinc"],
    name: "Kotlin",
    monacoLang: "kotlin",
    repl: `JAVA_OPTS="-Duser.home=$PWD" kotlinc`,
    main: "main.kts",
    run: `JAVA_OPTS="-Duser.home=$PWD" kotlinc -script main.kts; kotlinc`,
    template: `println("Hello, world!")
`,
    timeout: 30,
  },
  ksh: {
    aliases: ["kshell"],
    name: "Ksh",
    monacoLang: "shell",
    repl: `SHELL=/usr/bin/ksh HOME="$PWD" ksh`,
    input: "expr 123 * 234",
    main: ".kshrc",
    createEmpty: ``,
    run: `SHELL=/usr/bin/ksh HOME="$PWD" ksh`,
    scope: {
      code: `x="$(expr 123 * 234)"`,
      input: `echo "$x"`,
    },
    template: `echo "Hello, world!"
`,
  },
  less: {
    aliases: ["lessc"],
    name: "Less",
    monacoLang: "less",
    main: "main.less",
    run: "lessc main.less",
    format: {
      run: "prettier --no-config --stdin-filepath=format.less",
      input: `body:before {
    content: "Hello, world!";
}
`,
    },
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
    run: "lsc -r ./main.ls",
    template: `console.log "Hello, world!"
`,
    skip: ["repl", "runrepl"],
  },
  llvm: {
    name: "LLVM",
    monacoLang: "shell",
    main: "main.ll",
    compile: "clang -Wno-override-module main.ll -o main",
    run: "./main",
    template: `@.str = private unnamed_addr constant [13 x i8] c"Hello, world!"

declare i32 @puts(i8* nocapture) nounwind

define i32 @main() {
    %cast210 = getelementptr [13 x i8],[13 x i8]* @.str, i64 0, i64 0
    call i32 @puts(i8* %cast210)
    ret i32 0
}
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
    scope: {
      code: `x = 123 * 234`,
    },
    lsp: { start: "java -cp /usr/lib/EmmyLua-LS.jar com.tang.vscode.MainKt" },
    template: `print("Hello, world!")
`,
    skip: ["lsp"],
  },
  malbolge: {
    aliases: ["mb"],
    name: "Malbolge",
    main: "main.mb",
    run: "malbolge main.mb",
    hello: "Hello World!",
    template:
      " (=<`#9]~6ZY32Vx/4Rs+0No-&Jk)\"Fh}|Bcy?`=*z]Kw%oG4UUS0/@-ejc(:'8dc\n",
  },
  mariadb: {
    aliases: ["maria"],
    name: "MariaDB",
    repl: `rm -rf data && /opt/mariadb/scripts/mariadb-install-db --user="$(id -un)" && (/opt/mariadb/bin/mysqld --datadir="$PWD/data" --socket="$PWD/socket" --skip-networking &) && until [[ -e socket ]]; do sleep 0.01; done && mysql --socket="$PWD/socket"`,
    input: "SELECT 123 * 234;",
    main: "main.sql",
    run: `rm -rf data && /opt/mariadb/scripts/mariadb-install-db --user="$(id -un)" && (/opt/mariadb/bin/mysqld --datadir="$PWD/data" --socket="$PWD/socket" --skip-networking &) && until [[ -e socket ]]; do sleep 0.01; done && (mysql --socket="$PWD/socket" < main.sql; mysql --socket="$PWD/socket")`,
    lsp: {
      start: "sqls",
    },
    template: `SELECT 'Hello, world!';
`,
    skip: ["lsp"],
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
    format: {
      run: "prettier --no-config --stdin-filepath=format.md",
      input: `Hello, world!

`,
    },
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
    input: "SELECT 123 * 234;",
    main: "main.sql",
    run: `rm -rf data && mysqld -h "$PWD/data" --initialize-insecure && (mysqld -h "$PWD/data" --socket="$PWD/socket" --pid-file="$PWD/pid-file" --mysqlx=OFF --skip-networking &) && until [[ -e socket ]]; do sleep 0.01; done && (mysql --socket="$PWD/socket" -u root < main.sql; mysql --socket="$PWD/socket" -u root)`,
    lsp: {
      start: "sqls",
    },
    template: `SELECT 'Hello, world!';
`,
    timeout: 15,
    skip: ["lsp"],
  },
  nim: {
    name: "Nim",
    main: "main.nim",
    compile: "nim compile main.nim",
    run: "./main",
    template: `echo "Hello, world!"
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
    format: {
      run: "clang-format --assume-filename=format.m",
      input: `#import <Foundation/Foundation.h>

int main() {
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    NSLog(@"Hello, world!");
    [pool drain];
    return 0;
}
`,
    },
    lsp: {
      setup: `(gnustep-config --objc-flags && gnustep-config --base-libs) | sed -E 's/\\s+/\\n/g' > compile_flags.txt`,
      start: "clangd",
    },
    template: `#import <Foundation/Foundation.h>

int main() {
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  NSLog(@"Hello, world!");
  [pool drain];
  return 0;
}
`,
    skip: ["lsp"],
  },
  ocaml: {
    name: "OCaml",
    main: "main.ml",
    repl: "ocaml",
    input: "123 * 234 ;;",
    run: "ocaml -init main.ml",
    scope: {
      code: `;;
let x = 123 * 234`,
      input: `x ;;`,
    },
    format: {
      run: "touch .ocamlformat; ocamlformat --name=format.ml -",
      input: `print_string "Hello, world!\\n";;
`,
    },
    lsp: { start: "ocamllsp", lang: "ocaml" },
    template: `;;
print_string "Hello, world!\\n"
`,
    skip: ["lsp"],
  },
  octave: {
    aliases: ["matlab", "m", "mathworks"],
    name: "Octave",
    repl: "octave",
    main: "main.m",
    run: "octave --persist main.m",
    scope: {
      code: `x = 123 * 234`,
    },
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
    scope: {
      code: `my $x = 123 * 234;`,
      input: `$x`,
    },
    format: {
      run: "perltidy",
      input: `print ("Hello, world!\\n");
`,
    },
    template: `print("Hello, world!\\n");
`,
  },
  php: {
    aliases: ["phpcli"],
    name: "PHP",
    monacoLang: "php",
    repl: "php -a",
    input: "print 123 * 234;",
    main: "main.php",
    run: "php -d auto_prepend_file=main.php -a",
    scope: {
      code: `$x = 123 * 234;`,
      input: `echo $x;`,
    },
    lsp: { start: "intelephense --stdio" },
    template: `<?php

echo "Hello, world!\\n";
`,
    skip: ["lsp"],
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
    hello: "Hello World!",
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
    input: "SELECT 123 * 234;",
    main: "main.sql",
    run: `rm -rf data && /usr/lib/postgresql/*/bin/initdb -D data && (echo "listen_addresses = ''" && echo "unix_socket_directories = '.'") >> data/postgresql.conf && /usr/lib/postgresql/*/bin/pg_ctl -D data -w start && (psql -h "$PWD/data" postgres -f main.sql; psql -h "$PWD/data" postgres)`,
    lsp: {
      start: "sqls",
    },
    template: `SELECT 'Hello, world!';
`,
    timeout: 15,
    skip: ["lsp"],
  },
  powershell: {
    aliases: ["pwsh", "ps1"],
    name: "PowerShell",
    monacoLang: "powershell",
    repl: "SHELL=/usr/bin/pwsh pwsh",
    input: `expr 123 "*" 234`,
    main: "main.ps1",
    run: "SHELL=/usr/bin/pwsh pwsh -NoExit main.ps1",
    scope: {
      code: `Set-Variable x "$(expr 123 "*" 234)"`,
      input: `echo $x`,
    },
    lsp: {
      start: `pwsh -NoLogo -NoProfile -Command "/opt/powershell-editor-services/PowerShellEditorServices/Start-EditorServices.ps1 -BundledModulesPath /opt/powershell-editor-services -LogPath '$PWD/.powershell-editor-services/lsp.log' -SessionDetailsPath '$PWD/.powershell-editor-services/session.json' -FeatureFlags @() -AdditionalModules @() -HostName Riju -HostProfileId 'riju' -HostVersion 0.0 -Stdio -LogLevel Normal"`,
    },
    template: `Write-Host "Hello, world!"
`,
    skip: ["repl", "runrepl", "scope", "lsp"],
  },
  prolog: {
    name: "Prolog",
    repl: "prolog",
    input: "X is 123 * 234.",
    main: "main.pl",
    run: "prolog main.pl",
    scope: {
      code: `x(X) :- X is 123 * 234.`,
      input: `x(X).`,
    },
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
    timeout: 60,
  },
  python: {
    aliases: ["python3", "python2", "py"],
    name: "Python",
    monacoLang: "python",
    repl: "python3 -u",
    main: "main.py",
    run: "python3 -u -i main.py",
    scope: {
      code: `x = 123 * 234`,
    },
    format: {
      run: "black -",
      input: `print('Hello, world!')
`,
    },
    pkg: {
      install: "pip3 install --user NAME",
      uninstall: "pip3 uninstall NAME",
      search: `python3 -c 'import json; from xmlrpc import client; print(json.dumps(client.ServerProxy("https://pypi.org/pypi").search({"name": "NAME"})))' | jq -r 'map(.name) | .[]'`,
    },
    lsp: {
      start: "Microsoft.Python.LanguageServer",
      init: {
        interpreter: {
          properties: {
            InterpreterPath: "/usr/bin/python3",
          },
        },
      },
      code: "import func",
      item: "functools",
    },
    template: `print("Hello, world!")
`,
    timeout: 15,
  },
  قلب: {
    aliases: ["qalb"],
    name: "قلب",
    repl: "node /opt/qalb/repl.js",
    input: "(ضرب ١٢٣ ٢٣٤)",
    main: "main.qalb",
    hello: "مرحبا يا عالم",
    run: "node /opt/qalb/repl.js main.qalb",
    scope: {
      code: `(حدد خ (ضرب ١٢٣ ٢٣٤))`,
      input: `خ`,
    },
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
    scope: {
      code: `x = 123 * 234`,
    },
    template: `print("Hello, world!")
`,
  },
  racket: {
    aliases: ["rkt"],
    name: "Racket",
    repl: "racket",
    input: "(* 123 234)",
    main: "main.rkt",
    run: `racket -i -e '(enter! "main.rkt") (display "[ type (enter! \\"main.rkt\\") to access local variables ]\\n")'`,
    scope: {
      code: `(define x (* 123 234))`,
      input: `(enter! "main.rkt")
x`,
    },
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
    format: {
      run: "refmt",
      input: `print_string("Hello, world!\\n")
`,
    },
    lsp: {
      setup: `cp -a /opt/reasonml/project-template/* ./`,
      start: "reason-language-server",
    },
    template: `print_string("Hello, world!\\n");
`,
    skip: ["lsp"],
  },
  redis: {
    name: "Redis",
    monacoLang: "redis",
    repl:
      "rm -f socket; (redis-server --port 0 --unixsocket socket &); until [[ -e socket ]]; do sleep 0.01; done; redis-cli -s socket",
    input: `EVAL "return 123 * 234" 0`,
    main: "main.redis",
    run:
      "rm -f socket; (redis-server --port 0 --unixsocket socket &); until [[ -e socket ]]; do sleep 0.01; done; redis-cli -s socket < main.redis; redis-cli -s socket",
    template: `ECHO "Hello, world!"
`,
    skip: ["repl", "runrepl"],
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
    scope: {
      code: `x = 5`,
    },
    format: {
      run: "rufo -x",
      input: `puts "Hello, world!";
`,
    },
    pkg: {
      install: "gem install --user-install NAME",
      uninstall: "gem uninstall --user-install NAME",
      search: `curl -sS 'https://rubygems.org/api/v1/search.json?query=NAME' | jq -r 'map(.name) | .[]'`,
    },
    lsp: { start: "solargraph stdio" },
    template: `puts "Hello, world!"
`,
    skip: ["repl", "runrepl", "scope", "lsp"],
  },
  rust: {
    aliases: ["rs", "rustc"],
    name: "Rust",
    monacoLang: "rust",
    main: "main.rs",
    compile: "rustc main.rs",
    run: "./main",
    lsp: { start: "rls" },
    template: `fn main() {
    println!("Hello, world!");
}
`,
    skip: ["lsp"],
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
    scope: {
      code: `val x = 123 * 234`,
    },
    template: `println("Hello, world!")
`,
    timeout: 30,
  },
  scheme: {
    aliases: ["scm", "mitscheme"],
    name: "Scheme",
    monacoLang: "scheme",
    repl: "mit-scheme",
    input: "(* 123 234)",
    main: "main.scm",
    run: "mit-scheme --load main.scm",
    scope: {
      code: `(define x (* 123 234))`,
    },
    template: `(display "Hello, world!")
(newline)
`,
  },
  scss: {
    name: "SCSS",
    monacoLang: "scss",
    main: "main.scss",
    run: "sass main.scss",
    format: {
      run: "prettier --no-config --stdin-filepath=format.scss",
      input: `body:before {
    content: "Hello, world!";
}
`,
    },
    template: `body:before {
  content: "Hello, world!";
}
`,
  },
  sed: {
    aliases: ["gsed"],
    name: "Sed",
    main: "main.sed",
    helloInput: "",
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
    input: `expr 123 \\* 234`,
    main: ".profile",
    createEmpty: ``,
    run: `SHELL=/usr/bin/sh HOME="$PWD" posh -l`,
    scope: {
      code: `x="$(expr 123 \\* 234)"`,
      input: `echo "$x"`,
    },
    template: `echo "Hello, world!"
`,
  },
  shakespeare: {
    aliases: ["spl"],
    name: "Shakespeare",
    repl: "shakespeare console",
    input: `Hamlet, a placeholder.
done
stage`,
    output: "Off stage",
    main: "main.spl",
    hello: "Hello World!",
    suffix: "\n[A pause]",
    run: "shakespeare debug main.spl",
    runReplInput: "stage",
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
    timeout: 15,
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
    input: ` OUTPUT = 123 * 234
END`,
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
    input: "SELECT 123 * 234;",
    main: "main.sql",
    run: `sqlite3 -cmd "$(< main.sql)"`,
    lsp: {
      start: "sqls",
    },
    template: `SELECT 'Hello, world!';
`,
    skip: ["lsp"],
  },
  standardml: {
    aliases: ["sml", "ml"],
    name: "Standard ML",
    repl: "rlwrap sml",
    input: "123 * 234;",
    main: "main.sml",
    run: "rlwrap sml main.sml",
    scope: {
      code: `val x = 123 * 234;`,
      input: `x;`,
    },
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
    lsp: { start: "sourcekit-lsp" },
    template: `print("Hello, world!")
`,
    skip: ["lsp"],
  },
  tcl: {
    aliases: ["tclsh", "tclshrc"],
    name: "Tcl",
    monacoLang: "tcl",
    repl: "tclsh",
    input: "expr 123 * 234",
    main: ".tclshrc",
    createEmpty: ``,
    run: `HOME="$PWD" tclsh`,
    scope: {
      code: `set x [expr 123 * 234]`,
      input: `echo $x`,
    },
    template: `puts {Hello, world!}
`,
  },
  tcsh: {
    aliases: ["tcshell", "tcshrc", "csh"],
    name: "Tcsh",
    monacoLang: "shell",
    repl: `SHELL=/usr/bin/tcsh HOME="$PWD" tcsh`,
    input: `expr 123 \\* 234`,
    main: ".tcshrc",
    createEmpty: ``,
    run: `SHELL=/usr/bin/tcsh HOME="$PWD" tcsh`,
    scope: {
      code: "set x=`expr 123 \\* 234`",
      input: `echo "$x"`,
    },
    template: `echo "Hello, world!"
`,
  },
  tex: {
    aliases: ["latex", "xetex", "plaintex"],
    name: "TeX",
    repl: "tex",
    input: `\\newcount\\x
\\advance\\x by 123
\\multiply\\x by 234
\\message{\\the\\x}`,
    main: "main.tex",
    run: "tex main.tex",
    scope: {
      code: `\\newcount\\x
\\advance\\x by 123
\\multiply\\x by 234`,
      input: `\\message{\\the\\x}`,
    },
    lsp: { start: "digestif", lang: "tex" },
    template: `\\message{Hello, world!}
`,
    skip: ["lsp"],
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
    scope: {
      code: `let x = 123 * 234;`,
    },
    format: {
      run: "prettier --no-config --stdin-filepath=format.ts",
      input: `console.log('Hello, world!');
`,
    },
    template: `console.log("Hello, world!");
`,
    timeout: 15,
  },
  unlambda: {
    aliases: ["unl"],
    name: "Unlambda",
    repl: "unlambda-repl",
    input: "`.2`.8`.7`.8`.2i",
    main: "main.unl",
    run: "unlambda-repl main.unl",
    template: "`.\n`.!`.d`.l`.r`.o`.w`. `.,`.o`.l`.l`.e`.Hi\n",
  },
  vim: {
    aliases: ["viml", "vimscript"],
    name: "Vimscript",
    repl: "vim",
    input: ":echo 123 * 234",
    main: "main.vim",
    run: `vim -c "$(< main.vim)"`,
    scope: {
      code: `:let x = 123 * 234`,
      input: `:echo x`,
    },
    lsp: { start: "vim-language-server --stdio" },
    template: `:echo "Hello, world!"
`,
    skip: ["lsp"],
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
    timeout: 15,
  },
  whitespace: {
    aliases: ["ws"],
    name: "Whitespace",
    main: "main.ws",
    run: "whitespace main.ws",
    hello: "Hello, world",
    template: `Hello, world  \t  \t   \n\t\n     \t\t  \t \t\n\t\n     \t\t \t\t  \n\t\n     \t\t \t\t  \n\t\n     \t\t \t\t\t\t\n\t\n     \t \t\t  \n\t\n     \t     \n\t\n     \t\t\t \t\t\t\n\t\n     \t\t \t\t\t\t\n\t\n     \t\t\t  \t \n\t\n     \t\t \t\t  \n\t\n     \t\t  \t  \n\t\n  \n\n\n`,
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
    scope: {
      code: `x = 123 * 234`,
    },
    template: `Print["Hello, world!"]
`,
    timeout: 15,
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
    format: {
      run: "prettier --no-config --stdin-filepath=format.yaml",
      input: `output: 'Hello, world!'
`,
    },
    template: `output: "Hello, world!"
`,
  },
  zot: {
    name: "Zot",
    main: "main.zot",
    run: "zot --file main.zot",
    hello: `0100100001100101011011000110110001101111001011000010000001110111011011110111001001101100011001000010000100001010`,
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
    timeout: 30,
  },
  zsh: {
    aliases: ["zshell", "zshrc"],
    name: "Zsh",
    monacoLang: "shell",
    repl: "SHELL=/usr/bin/zsh zsh",
    input: `expr 123 \\* 234`,
    main: ".zshrc",
    createEmpty: ``,
    run: `SHELL=/usr/bin/zsh ZDOTDIR="$PWD" zsh`,
    scope: {
      code: `x="$(expr 123 \\* 234)"`,
      input: `echo "$x"`,
    },
    template: `echo "Hello, world!"
`,
  },
};
