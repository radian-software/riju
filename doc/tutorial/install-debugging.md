# Tutorial: debug language installation

Except for the simplest cases, it's often helpful to debug lanugage
installation interactively. Open a new tab in the admin shell
(`control-b c`) and start the packaging image:

```
$ make shell I=packaging
```

You'll probably want another instance of tmux:

```
$ make tmux
```

Now you should have at least three tabs in the top-level tmux: one
with the admin shell, one with the runtime shell, and one with the
packaging shell.

## Targeted debugging commands

Switch to the packaging shell and delete any leftover artifacts from
the previous build:

```
$ make pkg-clean T=lang L=mylanguage
```

Then start a shell in the same context as the `install` scripts would
be run.

```
$ make pkg-debug T=lang L=mylanguage
```

Now you can run installation commands manually to make sure they're
working. Normally the generated build script will be run; you can see
it by running

```
$ make script T=lang L=mylanguage
```

and checking `build/lang/<mylanguage>/build.bash`. You can also run
that script in the appropriate context before entering your
`pkg-debug` session:

```
$ make pkg-build T=lang L=mylanguage
```

After you're satisfied with the layout of files you've put into
`${pkg}` and want to try installing the resulting package, start by
building the `.deb`:

```
$ make pkg-deb T=lang L=mylanguage
```

Then switch to the runtime shell. If you still only have one tab open
inside the nested tmux session, open a new one (`control-b control-b
c`). You can install the `.deb`:

```
$ make install T=lang L=mylanguage
```

At this point your language should be runnable so you can test it out.
It's best to test inside an isolated sandbox with the same set of
environment variables as will appear on Riju, though; you can start
such a shell as follows:

```
$ make sandbox L=mylanguage
```

You may find that the language fails to run as expected due to
packaging errors. If so, you can return to the packaging shell and
make adjustments inside `pkg-debug` before rebuilding and reinstalling
the package.
