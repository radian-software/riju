# Tutorial: provide run commands

Now that your language is installed you need to tell Riju how to run
it. Here's an example for Dart:

```yaml
main: "main.dart"
template: |
  void main() {
    print('Hello, world!');
  }

run: |
  dart main.dart
```

Note:

* The contents of `template` are put into the `main` filename, and
  `run` is expected to run that file.
* The `main` filename should follow existing conventions for your
  language, typically `main.foo` where `foo` is a standard file
  extension. If there's no standard file extension you can pick a
  reasonable-sounding one, like `main.beatnik` for Beatnik. You can
  use subdirectories (e.g. `src/main.foo`) if needed, but this is
  pretty rare.
* The `template` code should print exactly the text `Hello, world!`
  with a trailing newline to stdout, or as close to that as possible.

## Compiled languages

If your language has a separate compilation step that produces a
binary or other intermediate artifact, you can add a separate
`compile` command; for example:

```yaml
main: "Main.java"
template: |
  public class Main {
      public static void main(String[] args) {
          System.out.println("Hello, world!");
      }
  }

compile: |
  javac Main.java
run: |
  java Main
```

There is no hard requirement on the names of intermediate files. In
the case of Java, the intermediate file is named `Main.class`, with
the `java` command appending the `.class` part implicitly.

## Languages with REPLs

If your language has the ability to run an interactive session, you
can expose that functionality via the `repl` key. Here is the desired
behavior for languages with REPLs:

* The `repl` command will start an interactive session *without* any
  user code loaded
* The `run` command will execute the user code in `main`, like before,
  and will *then* start an interactive session, preferably in the same
  environment (so variables are still in scope, for example)

Here is an example for Python (`-u` is to prevent output buffering):

```yaml
repl: |
  python3 -u

main: "main.py"
template: |
  print("Hello, world!")

run: |
  python3 -u -i main.py
```

## Preserving variable scope

In the Python example above, passing `-i main.py` to `python3` starts
an interactive session where `main.py` is executed, and then any
variables defined in `main.py` can be inspected at the REPL. This is
the desired state of operation. Many interactive languages provide
some command-line option(s) to achieve a similar effect, although they
may be a bit obscure. For example, in Node.js, you have to pass the
program as a string:

```yaml
repl: |
  node

main: "main.js"
template: |
  console.log("Hello, world!");

run: |
  node -e "$(< main.js)" -i
```

Or you may be able to abuse a "startup" or "rc" file that is loaded at
startup in a special way. For example, Haskell's interpreter evaluates
`.ghci` specially at startup:

```yaml
repl: |
  rm -f .ghci
  ghci

main: "Main.hs"
template: |
  module Main where

  main :: IO ()
  main = putStrLn "Hello, world!"

run: |
  (echo ':load Main' && echo 'main') > .ghci && ghci
```

In the case of shells, we often want to put the code itself into the
startup file:

```yaml
repl: |
  SHELL=/usr/bin/zsh HOME="$PWD" zsh
input: |
  expr 123 \* 234

main: ".zshrc"
template: |
  echo "Hello, world!"
createEmpty: ""

run: |
  SHELL=/usr/bin/zsh HOME="$PWD" zsh
```

In the above example, we passed `createEmpty: ""`. To explain, the
default behavior is for user code to be written into `main`
immediately, even before the user has clicked Run. This is so that
certain language servers will work correctly. However, in cases where
we're abusing a startup file, it may not be appropriate. After all,
`zsh` will load `.zshrc` no matter what (note that `repl` and `run`
are identical here). By passing `createEmpty: ""`, we make it so that
Riju will write the provided string (i.e. `""`) into `main` before
executing `repl`. This ensures that user code is *not* run when
starting a REPL, but only when actually running.

Sometimes it's simply not possible to preserve variable scope from
user code when starting an interactive session. In that case, we just
start the interactive session separately. It's important to start the
interactive session regardless of whether or not the user code had an
error (so use `;` instead of `&&`):

```yaml
repl: |
  zoem

main: "main.azm"
template: |
  \inform{Hello, world!}

run: |
  zoem -I main.azm; zoem
```
