# Tutorial: add a code formatter

Not all languages have code formatters, but if they exist, we like to
add them. You'll need to update the `install` recipe in your
language's configuration to install the code formatter as well. Then
add a `format.run` key with a shell command that will read a program
on stdin and write the formatted version to stdout.

You'll also want to add a `format.input` key which is equivalent to
the `template` code, but formatted incorrectly. This can be used to
verify that the formatter is working as expected.

Here's an example:

```yaml
install:
  apt:
    - black

template: |
  print("Hello, world!")

format:
  run: |
    black -
  input: |
    print('Hello, world!')
```
