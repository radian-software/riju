# Riju

Riju is a very fast online playground for every programming language.
In less than a second, you can start playing with a Python interpreter
or compiling INTERCAL code.

Check out the [live application](https://riju.codes/)!

**You should not write any sensitive code on Riju, as NO GUARANTEES
are made about the security or privacy of your data. (No warranty etc
etc.)**

This project is a work in progress, and I don't intend on thoroughly
documenting it until it has reached feature-completeness.

## Criteria for language inclusion

I aspire for Riju to support more languages than any reasonable person
could conceivably think is reasonable. That said, there are some
requirements:

* **Language must have a clear notion of execution.** This is because
  a core part of Riju is the ability to execute code. Languages like
  [YAML](https://yaml.org/), [SCSS](https://sass-lang.com/), and
  Markdown are fine because they have a canonical transformation (into
  [JSON](https://www.json.org/json-en.html),
  [CSS](https://developer.mozilla.org/en-US/docs/Web/CSS), and
  [HTML](https://developer.mozilla.org/en-US/docs/Web/HTML)
  respectively) that can be performed on execution. However, languages
  like JSON, CSS, and HTML are not acceptable, because there's nothing
  reasonable to do when they are run.
* **Language must not require input or configuration.** This is
  because, in order to avoid bloating the interface, Riju provides a
  way to supply code but not any other data. Of course, it's possible
  to supply input interactively, so reading stdin is allowed, but if a
  language can only reasonably be programmed with additional input,
  it's not a candidate for inclusion. Thus, many templating languages
  are excluded, since they don't do anything unless you are
  substituting a value. However, some languages such as
  [Pug](https://pugjs.org/) are allowed, because they implement a
  significant syntax transformation outside of template substitution.
  Also, languages like [Sed](https://www.gnu.org/software/sed/) and
  [Awk](https://www.gnu.org/software/gawk/) are allowed, because it's
  straightforward to test code written in them even without a
  pre-prepared input file.
* **Language must not require a graphical environment.** This is
  because we use a pty to run code, and there is no X forwarding. As
  such, we can't use languages like
  [Scratch](https://scratch.mit.edu/),
  [Alice](https://www.alice.org/), and
  [Linotte](http://langagelinotte.free.fr/wordpress/).
* **Language must be available for free under a permissive license.**
  This is because we must download and install all languages
  noninteractively in the Docker image build, so anything that
  requires license registration is unlikely to work (or be legal). We
  can't use [Mathematica](https://www.wolfram.com/mathematica/) or
  [MATLAB](https://www.mathworks.com/products/matlab.html), for
  example, but we can use [Mathics](https://mathics.github.io/) and
  [Octave](https://www.gnu.org/software/octave/), which provide
  compatible open-source implementations of the underlying languages.
* **Language must be runnable under Docker on Linux.** This is because
  that's the execution environment we have access to.
  [AppleScript](https://en.wikipedia.org/wiki/AppleScript) is out
  because it only runs on macOS, and [Docker](https://www.docker.com/)
  is out because it can't be run inside Docker (without the
  `--privileged` flag, which has unacceptable security drawbacks; see
  [#29](https://github.com/raxod502/riju/issues/29)). Note, however,
  that many Windows-based languages can be used successfully via
  [Mono](https://www.mono-project.com/) or
  [Wine](https://www.winehq.org/), such as
  [Cmd](https://en.wikipedia.org/wiki/Cmd.exe),
  [C#](https://en.wikipedia.org/wiki/C_Sharp_(programming_language)),
  and [Visual Basic](https://en.wikipedia.org/wiki/Visual_Basic).

Here are some explicit *non-requirements*:

* *Language must be well-known.* Nope, I'll be happy to add your pet
  project; after all, [Kalyn](https://github.com/raxod502/kalyn) and
  [Ink](https://github.com/thesephist/ink) are already supported.
* *Language must be useful.* I would have no objection to adding
  everything on the esolangs wiki, if there are interpreters/compilers
  available.
* *Language must be easy to install and run.* Well, it would be nice,
  but I've seen some s\*\*\* when adding languages to Riju so it will
  take a lot to surprise me at this point.

## Project setup

To run the webserver, all you need is Yarn. Just run `yarn install` as
usual to install dependencies. For production, it's:

    $ yarn backend    |- or run all three with 'yarn build'
    $ yarn frontend   |
    $ yarn system     |
    $ yarn server

For development with file watching and automatic server rebooting and
all that, it's:

    $ yarn backend-dev    |- or run all four with 'yarn dev'
    $ yarn frontend-dev   |
    $ yarn system-dev     |
    $ yarn server-dev     |

The webserver listens on `localhost:6119`. Now, although the server
itself will work, the only languages that will work are the ones that
happen to be installed on your machine. (I'm sure you can find a few
that are already.) Also, sandboxing using UNIX filesystem permissions
will be disabled, because that requires root privileges. If you want
to test with *all* the languages plus sandboxing (or you're working on
adding a new language), then you need to use Docker. Running the app
is exactly the same as before, you just have to jump into the
container first:

    $ make docker

Note that building the image typically requires over an hour and 20 GB
of disk space, and it is only growing.

The above command generates the development image as a subroutine. You
can skip this and use the last tagged development image:

    $ make docker-nobuild

Or you can explicitly build the image without running it:

    $ make image-dev

The production image is based on the development one, with some
additional layers. You can build it as follows:

    $ make image-prod

Lastly I should mention the tests. There are integration tests for
every language, and they can be run as follows:

    $ [CONCURRENCY=2] [TIMEOUT_FACTOR=1] yarn test [<filter>...]

Filters can be for language (`python`, `java`) or test type (`hello`,
`lsp`). You can comma-delimit multiple filters to do a disjunction,
and space-delimit them to do a conjunction (`yarn test hello
python,java` for the `hello` tests for `python` and `java`).

The tests are run automatically when building the production image,
and fail the build if they fail.

See also [riju-cdn](https://github.com/raxod502/riju-cdn).

## Flag

[![Flag](flag.png)](https://www.reddit.com/r/Breath_of_the_Wild/comments/947ewf/flag_of_the_gerudo_based_on_the_flag_of_kazakhstan/)
