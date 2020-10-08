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

If you'd like to request a new language, head to the [language support
meta-issue](https://github.com/raxod502/riju/issues/24) and add a
comment. Of course, if you actually want it to be added anytime soon,
you should submit a pull request :)

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

## Adding a language

The workflow for adding a language is more streamlined than you might
expect, given that building Riju's Docker image takes over an hour.
This is because there is no need to rebuild the image when a change is
made. Instead, you can manually apply the changes to a running
container in parallel with adding those changes to the Dockerfile
scripts.

### Install

The first step in adding a language is figuring out how to install it.
There are a number of considerations here:

* If it's available from Ubuntu, that's the best option.
* Language-specific package managers are a second-best choice.
* Downloading precompiled binaries is also not the worst. It's best if
  upstream offers a .deb download, but manual installation is fine
  too.
* Compiling from source is the worst option, but sometimes it's the
  only way.

Typically, I `sudo su` and change directory to `/tmp` in order to test
out installation. Once I've identified a way to install such that the
software appears to function, I transcribe the commands from my shell
back into the relevant Dockerfile script.

#### Dockerfile scripts

These are as follows:

* `docker-install-phase0.bash`: perform initial upgrade of all Ubuntu
  packages, unminimize system
* `docker-install-phase1.bash`: configure APT repositories and
  additional architectures
* `docker-install-phase2.bash`: install tools that are used for Riju
  itself (build and development tools)
* `docker-install-phase3a.bash`: install APT packages for languages
  A-D
* `docker-install-phase3b.bash`: install APT packages for languages
  E-L
* `docker-install-phase3c.bash`: install APT packages for languages
  M-R
* `docker-install-phase3d.bash`: install APT packages for languages
  S-Z
* `docker-install-phase4.bash`: install precompiled binaries and
  tarballs
* `docker-install-phase5.bash`: set up language-specific package
  managers and install packages from them
* `docker-install-phase6.bash`: install things from source
* `docker-install-phase7.bash`: set up project templates for languages
  that require you start by running a "create new project" command,
  and install custom wrapper scripts
* `docker-install-phase8.bash`: set up access control and do final
  cleanup

#### Rolling-release policy

You'll notice in these scripts a distinct lack of any version numbers.
This is because Riju uses rolling-release for everything that can
conceivably be rolling-released (even things that look like they're
probably never *going* to get a new release, since the last one was in
2004).

For APT and language-specific packages, this is typically simple. A
small number of APT packages include a version number as part of their
name for some reason, and I work around this using various
`grep-aptavail` incantations at the top of the `phase-3[ad].bash`
scripts. I suggest checking those examples and referring to the
`grep-aptavail` man page to understand what is going on.

For binaries and tarballs in `phase4.bash`, a version number is
typically encoded in the download URL. For projects available via
GitHub Releases (preferred), there is a `latest_release` shell
function to fetch the latest tag. For things hosted elsewhere, I
resort to using `curl` and `grep` on the download homepage to identify
the latest version number or download URL. Crafting an appropriate
pipeline for these cases is as much an art as a science. We simply
hope that the relevant webpages will not have their layout changed too
frequently.

#### Conventions

* We do all work from `/tmp` and clean up our files when done. (The
  current code doesn't always do a great job of this; see
  [#27](https://github.com/raxod502/riju/issues/27).)
* When changing directory, we use `pushd` and `popd` in pairs.
* We prefer putting files where they're supposed to be in the first
  place, rather than moving (or worse, copying) them. This can be
  accomplished by means of `wget -O`, `unzip -d`, `tar -C
  [--strip-components]`, and similar.
* We like to keep things as minimal as possible in terms of shell
  scripting, but try to follow the standard installation procedure
  where reasonable.

### Running

There are three categories of languages: non-interactive, interactive,
and interactive+scoped. The capabilities are as follows:

* *Non-interactive:* You can run a file.
* FIXME

## Debugging tools

Add `#debug` to the end of a Riju URL and reload the page to output
all messages in JSON format in the JavaScript console. You can copy
the LSP messages as JSON for direct use in the LSP REPL (see below).

To get a sandboxed shell session, the same as is used to run languages
on Riju, run:

    $ yarn sandbox

To start up a JSON REPL for interacting with LSP servers, run:

    $ yarn lsp-repl (LANGUAGE | CMD...)

## Self-hosting

Riju is hosted on [DigitalOcean](https://www.digitalocean.com/). Sign
up for an account and obtain a personal access token with read/write
access.

You will need some credentials. Start by selecting an admin password
to use for the DigitalOcean instance. Then generate two SSH key-pairs
(or you can use pre-existing ones). One is for the admin account on
DigitalOcean, while the other is to deploy from CI.

Install [Packer](https://www.packer.io/). Riju uses Packer to generate
DigitalOcean AMIs to ensure a consistent setup for the production
instance. Navigate to the `packer` subdirectory of this repository and
create a file `secrets.json`, changing the values as appropriate for
your setup:

```json
{
  "digitalocean_api_token": "28114a9f0ed5637c576794138c71bf03d01946288a6922ea083f923ec883c431",
  "admin_password": "R3iIhqs856N1sT5Mg6QFAsB5VPJrXS",
  "admin_ssh_public_key_file": "/home/raxod502/.ssh/id_rsa.pub",
  "deploy_ssh_public_key_file": "/home/raxod502/.ssh/id_rsa_riju_deploy.pub"
}
```

We'll start by setting up Riju without TLS. Run:

    $ packer build -var-file secrets.json config.json

This will take about five minutes to generate a DigitalOcean AMI. Log
in to your DigitalOcean and launch an instance based on that AMI
(called an "Image" in the interface). The hosted version of Riju uses
the $10/month instance with 1 vCPU and 2GB memory / 50GB disk.

Root login is disabled on the AMI generated by Packer, but
DigitalOcean unfortunately doesn't give you any option to leave login
settings unchanged. I suggest setting the root password to a random
string. Make a note of the IP address of the droplet and SSH into it
under the admin user, using the key that you specified in
`secrets.json`. Now perform the following setup:

    $ sudo passwd -l root

This completes the first DigitalOcean portion of deployment.

Now you'll need an account on [Docker Hub](https://hub.docker.com/),
which is where built images will be stored before they are pulled down
to DigitalOcean. Create a repository; the name will be
`your-docker-id/whatever-you-name-the-repo`. You'll need this below.

You're now ready to deploy. You can do this manually to begin with. In
the repository root on your local checkout of Riju, create a file
`.env`, changing the values as appropriate for your setup:

    DOCKER_REPO=raxod502/riju
    DOMAIN=riju.codes
    DEPLOY_SSH_PRIVATE_KEY=/home/raxod502/.ssh/id_rsa_riju_deploy

Run:

    $ docker login
    $ make deploy

Riju should now be available online at your instance's public IP
address.

Next, let's configure TLS. You'll need to configure DNS for your
domain with a CNAME to point at your DigitalOcean instance. Once DNS
has propagated, SSH into your DigitalOcean instance and run:

    $ sudo systemctl stop riju
    $ sudo certbot certonly --standalone
    $ sudo systemctl start riju

You'll also want to set up automatic renewal. This can be done by
installing the two Certbot hook scripts from Riju in the
`packer/resources` subdirectory. Here is one approach:

    $ sudo wget https://github.com/raxod502/riju/raw/master/packer/resources/certbot-pre.bash
             -O /etc/letsencrypt/renewal-hooks/pre/riju
    $ sudo wget https://github.com/raxod502/riju/raw/master/packer/resources/certbot-post.bash
             -O /etc/letsencrypt/renewal-hooks/post/riju
    $ sudo chmod +x /etc/letsencrypt/renewal-hooks/pre/riju
    $ sudo chmod +x /etc/letsencrypt/renewal-hooks/post/riju

At this point you should be able to visit Riju at your custom browser
with TLS enabled.

We can now set up CI. Sign up at [CircleCI](https://circleci.com/) and
enable automatic builds for your fork of Riju. You'll need to set the
following environment variables for the Riju project on CircleCI,
adjusting as appropriate for your own setup:

    DOCKER_USERNAME=raxod502
    DOCKER_PASSWORD=MIMvzS1bKPunDDSX4AJu
    DOCKER_REPO=raxod502/riju
    DOMAIN=riju.codes
    DEPLOY_SSH_PRIVATE_KEY=b2Rs......lots more......SFAK

To obtain the base64-encoded deploy key, run:

    $ cat ~/.ssh/id_rsa_riju_deploy | base64 | tr -d '\n'; echo

New pushes to master should trigger deploys, while pushes to other
branches should trigger just builds.
