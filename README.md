# Riju

Riju is a very fast online playground for every programming language.
In less than a second, you can start playing with a Python interpreter
or compiling INTERCAL code.

Check out the [live application](https://riju-sandbox.herokuapp.com/)!

**You should not write any sensitive code on Riju, as NO GUARANTEES
are made about the security or privacy of your data. (No warranty etc
etc.)**

This project is a work in progress, and I don't intend on thoroughly
documenting it until it has reached feature-completeness.

## Project setup

To run the webserver, all you need is Yarn. Just run `yarn install` as
usual to install dependencies. For production, it's:

    $ yarn backend
    $ yarn frontend
    $ yarn server

For development with file watching and automatic server rebooting and
all that, it's:

    $ yarn backend-dev
    $ yarn frontend-dev
    $ yarn server-dev

The webserver listens on `localhost:6119`. Now, although the server
itself will work, the only languages that will work are the ones that
happen to be installed on your machine. (I'm sure you can find a few
that are already.) If you want to test with *all* the languages (or
you're working on adding a new language), then you need to use Docker.
Running the app is exactly the same as before, you just have to jump
into the container first:

    $ make docker

Note that building the image takes 30 minutes and requires about 15 GB
of disk space.

## Flag

[![Flag](flag.png)](https://www.reddit.com/r/Breath_of_the_Wild/comments/947ewf/flag_of_the_gerudo_based_on_the_flag_of_kazakhstan/)
