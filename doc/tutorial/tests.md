# Tutorial: Configure tests

Riju has far too many languages to rely on manual testing to ensure
that features are not broken by upstream updates. As such, we have a
system to automatically generate test cases which are validated
whenever language configuration changes.

Here are the tests that can be configured for each language:

* `run` (mandatory for all languages): Verify that the `template` code
  prints `Hello, world!` when run using the `run` command.
* `repl` (mandatory for all languages with `repl`): Verify that
  submitting `123 * 234` to the `repl` command causes `28782` to be
  printed.
* `runrepl` (mandatory for all languages with `repl`): Same as `repl`,
  but using the `run` command instead of the `repl` command (i.e.,
  test that the `run` command starts a REPL after executing `main`).
* `scope` (optional, only for languages with variable-scope-preserving
  `repl`): Verify that if a variable assignment is appended to `main`,
  then that variable can be evaluated from the REPL using the `run`
  command.
* `format` (mandatory for all languages with `format`): Verify that a
  misformatted version of the `template` code is correctly formatted
  back to its canonical form when executing `format.run`.
* `lsp` (mandatory for all languages with `lsp`): Verify that the
  language server produces a given autocompletion in a given context.
* `ensure` (optional): Verify that a specific shell command executes
  successfully. This is currently unused.

## Test configuration

See [`jsonschema.yaml`](../../lib/jsonschema.yaml) for full
documentation.

* `run`
    * If your language can't be made to print exactly `Hello, world!`,
      specify the actual output using the `hello` key.
        * In extraordinary circumstances, a language may be unable to
          produce deterministic output (e.g. Entropy). In such cases,
          `hello` can also be a JavaScript-compatible regular
          expression, and you must specify `helloMaxLength` which is
          the maximum possible length of the `Hello, world` output
          matched by the regex.
    * Some languages require user input at the REPL to run the code,
      despite our best efforts to the contrary. In this case, you can
      specify the required input in the `helloInput` key. (See
      "Specifying user input" below.)
    * If a language *doesn't* have `repl`, then the `run` command is
      expected to terminate after executing user code. By default the
      expected exit status is 0, and the `run` test will fail
      otherwise. If for some reason your language exits with a nonzero
      status even in the absence of an error, then you can specify the
      expected status in the `helloStatus` key.
* `repl`
    * We try to compute `123 * 234` in most languages' REPLs.
      Naturally, the syntax may vary depending on the language, so you
      can specify an alternate input using the `input` key. (See
      "Specifying user input" below.)
    * The result of `123 * 234` is generally `28782`. In the case that
      we get the output in some other format, you can specify the
      expected output using the `output` key.
* `runrepl`
    * In the case that `input` needs to be different for `runrepl`
      than for `repl`, you can override it specifically for `runrepl`
      using the `runReplInput` key.
    * In the case that `output` needs to be different for `runrepl`
      than for `repl`, you can override it specifically for `runrepl`
      using the `runReplOutput` key.
* `scope`
    * *Required:* In `scope.code`, specify the code that is needed to
      assign `123 * 234` to a local variable named `x`, or as close to
      that as the language can manage. For example, in Python, this
      would be `x = 123 * 234`.
    * By default, `scope.code` is appended at the end of `template`.
      However, if it needs to go in the middle, you can specify
      `scope.after`, which should match an entire line of `template`.
      Then `scope.code` will be placed on the next list after
      `scope.after`.
    * By default, it's expected that typing `x` into the REPL will
      produce `28782`. You can override the input to something else by
      specifying `scope.input`. (See "Specifying user input" below.)
    * If the expected output is something other than `28782`, you can
      override it using `scope.output`.
* `format`
    * *Required:* In `format.input`, specify the input code. This
      should be distinct from `template`, but should turn into
      `template` when the `format` command is run on it.
    * In the case that you can't come up with input that formats to
      `template`, you can specify `format.output` as the expected
      result of formatting `format.input`.
* `lsp`
    * *Required:* In `lsp.code`, specify input (not necessarily an
      entire line) that we should pretend the user has typed. We
      expect an autocompletion to be presented with the cursor at the
      end of this input.
    * *Required:* In `lsp.item`, specify the text of an autocompletion
      that we expect the language server to generate. This should not
      match any text that actually appears in `template` or
      `lsp.code`.
    * In `lsp.after`, you can specify a string that will match exactly
      one place in `template`. This is where the cursor will be
      positioned before `lsp.code` is inserted. If `lsp.after` is not
      specified, then `lsp.code` will be inserted at the end of
      `template` on a new line.
* `ensure`
    * If you want to use an `ensure` test, just supply the shell
      command using the `ensure` key.

## Specifying user input

We have a couple different formats for common types of user input.
This will type `eval` and send a newline:

```yaml
input: |
  eval
```

This will type `eval`, send a newline, then type `go` and send another
newline:

```yaml
input: |
  eval
  go
```

This will type `foo`, send a newline, wait 1 second, then type `bar`
and send another newline:

```yaml
input: |
  foo
  DELAY: 1
  bar
```

Why is this useful? Unfortunately, many languages have race conditions
and will fail to notice input if you send it before they have finished
starting up.

Finally, this will type `foo` and then send a newline followed by an
EOF:

```yaml
input: |
  foo
  EOF
```

## Broken tests

We try very hard to get tests working for every newly added language.
However, sometimes there's something truly puzzling going on that's
not worth blocking a new language being added. For that reason it's
possible to mark a test as temporarily skipped, e.g.:

```yaml
skip:
  - repl
  - runrepl
  - scope
  - lsp
```

This is unfortunately currently the case for many of the LSP tests due
to the fragility of most language servers.
