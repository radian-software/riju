# Tutorial: debug language installation

Except for the simplest cases, it's often helpful to debug lanugage
installation interactively. Open a new tab in the admin shell and
start the packaging shell:

```
$ make shell I=packaging
```

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

Then switch back to the admin shell and run `dep
image:lang-mylanguage`, as usual. This should use the Debian package
you just built. Now you can test either in the Riju interface or via
`make sandbox L=mylanguage` from the admin shell.

You may find that the language fails to run as expected due to
packaging errors. If so, you can return to the packaging shell and
make adjustments inside `pkg-debug` before rebuilding and reinstalling
the package.
