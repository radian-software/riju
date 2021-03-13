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

## Variable scope
