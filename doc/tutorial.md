# Adding your own language to Riju

Hello and welcome! This tutorial guides you through the basics of
adding a new language to Riju, or modifying an existing language. The
other documentation in this repo has reference material that may be
helpful for advanced use cases, but this page should get you started.

If you run into any trouble following the guide, please do not
hesitate to open an issue!

## Project setup

Fork this repository to your account on GitHub, and clone it locally:

```
$ git clone https://github.com/yourname/riju.git
$ cd riju
```

Install [Docker](https://www.docker.com/). Then you can build and
start the admin shell:

```
$ make image shell I=admin
```

All future operations can be done inside the admin shell, where all
dependencies are installed automatically.

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

## Configure local project

Using your regular text editor (the Riju repository is synchronized
inside and outside of the container, so you can use whatever editor
you would like, it doesn't have to be something in the terminal),
create a file `.env` in the Riju repository with the following
contents:

```
DOCKER_REPO=raxod502/riju
S3_BUCKET=riju
```

This tells Riju to pull assets from the official registries that I
maintain, so that you don't have to build them yourself.

## Set up Docker images

Download the two Docker images needed for testing a new language:

```
$ make pull I=packaging
$ make pull I=runtime
```

Create a new tab in tmux (`control-b c`) and start the runtime image
with ports exposed:

```
$ make shell I=runtime E=1
```

Inside that shell, start another instance of tmux:

```
$ make tmux
```

Now within that tmux, start Riju in development mode:

```
$ make dev
```

You should now be able to navigate to <http://localhost:6119> and see
that Riju is running, although it does not have any languages
installed.

Finally, switch back to the admin shell (`control-b p`). We are ready
to start creating your new language.

## Create a language configuration

Create a file `langs/mylanguage.yaml` with the following contents:

```yaml
id: "mylanguage"
name: "My Language"

main: "TODO"
template: |
  # Fill this in later

run: |
  echo "Hello, world!"
```

Now from the admin shell, run `make repkgs L=mylanguage`. Once that
completes, you should see your language at <http://localhost:6119>.
Furthermore, you can switch to the runtime image (`control-b n`) and
run `make sandbox L=mylanguage` to test your language at the command
line (e.g. type `run` to print `Hello, world!`). Each time you modify
the language configuration, run `make repkgs` to reinstall the
language.

Follow these steps to augment your language configuration:

* [Install your language](tutorial/install.md)
* [Provide run commands](tutorial/run.md)
* [Configure tests](tutorial/tests.md)
* [Provide metadata](tutorial/metadata.md)
* [Add code formatter](tutorial/formatter.md)
* [Add language server](tutorial/lsp.md)
