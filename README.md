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
