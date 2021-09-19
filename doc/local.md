# Running Riju locally

This document has the steps necessary to build and run Riju locally,
without modifications. See the other documentation pages for
information about customizing and rebuilding Riju, or extending it by
adding or modifying the supported languages.

If you run into any trouble following the guide, please do not
hesitate to open an issue!

## Project setup

Clone locally:

```
$ git clone https://github.com/raxod502/riju.git
$ cd riju
```

Install [Docker](https://www.docker.com/). Then you can build and
start the admin shell:

```
$ make image shell I=admin
```

All future operations can be done inside the admin shell, where Riju's
dependencies are already installed.

## Start tmux

Start a tmux session:

```
$ make tmux
```

If you don't know how to use tmux, see [a
cheatsheet](https://danielmiessler.com/study/tmux/). The useful
keybindings are:

* `control-b c`: open new tab
* `control-b p/n`: previous/next tab
* `control-b "`: split tab into top and bottom panes
* `control-b %`: split tab into left and right panes
* `control-b <arrows>`: move between panes
* `control-b control-b <something>`: if you have two tmuxes nested,
  use `control-b` twice to do a command on the inner one instead of
  the outer one

## Fetch base Ubuntu image

Make sure you're using the same version of Ubuntu as the mainline
Riju:

```
$ make sync-ubuntu
```

## Start Riju server

Use `dep`, the Riju build tool, to compile the Docker image that the
Riju server will run inside:

```
$ dep image:runtime
```

Start Riju in development mode:

```
$ make shell I=runtime E=1 CMD="make dev"
```

You should now be able to navigate to <http://localhost:6119> and see
that Riju is running, although it does not have any languages
installed.

## Build languages

Building all languages supported by Riju is a lengthy process which
often requires some debugging as something upstream has broken since
the last time I built everything. You may be more interested in
getting your favorite language(s) up and running. To do that, find the
relevant language (say `foo.yaml`) in the `langs` subdirectory, and
run:

```
$ dep image:lang-foo
```

After this completes successfully, you'll automatically be able to use
that language in the web interface without needing to restart.

You can also run:

```
$ dep test:lang-foo
```

This will, in addition to building the language image, run the defined
integration tests to make sure it is functioning properly.

If you *do* want to build and test all supported languages, run:

```
$ dep deploy:ready
```

Expect this to take many hours.
