# Tutorial: add a language server

Language servers provide autocompletion and other handy features. They
are often extremely fiddly to get working, unfortunately.

Here's an example of the best-case scenario, where you can just
install a language server and it works out of the box:

```yaml
install:
  npm:
    - vim-language-server

lsp:
  start: |
    vim-language-server --stdio
  code: "TODO"
  item: "TODO"
```

Unfortunately it's usually not quite so easy, which is why we have
various configuration options (check existing languages for usage
examples):

* `setup`: Shell command to run to set up language server caches or
  whatever. This happens before `start`, once.
* `disableDynamicRegistration`: By default language server client
  "features" are registered one at a time with the server. Some
  servers are buggy and don't support the protocol correctly, which
  means setting this key to true may fix the problem.
* `init`, `config`: Two different ways of sending an arbitrary
  configuration blob to the language server. Sometimes a language
  server will need one or the other of them to be set to some
  particular value "because that's what VSCode does", or it won't work
  properly.
* `lang`: For some reason the client sends its impression of what
  language the current file is in to the server. This really shouldn't
  make a difference, but sometimes servers will barf if the magic
  string isn't quite right. In that case you can override it with the
  `lang` key.
* `code`, `after`, `item`: These are used in the test suite (see
  later).
