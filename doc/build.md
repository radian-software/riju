# Riju build system

Riju's build system is complex and takes some time to explain. Bear
with me. (If you just want to add or modify a language, you can read
the [tutorial](tutorial.md) instead.)

## Depgraph

The high level interface to Riju's build system is a tool called
Depgraph, which knows about all the build artifacts and has advanced
mechanisms for determining which of them need to rebuild based on
content hashes. Normally you can just use Depgraph to build the
artifacts you need. However, in some cases you may want to interact
with the lower level for more precise operations. This is done via
Makefile. (Furthermore, there are a few one-off artifacts such as the
admin image which are not part of the main build system, which means
that they are managed directly by Makefile.)

### Available build artifacts

* `image:ubuntu`: A fixed revision of the upstream `ubuntu:rolling`
  image that is used as a base for all Riju images.
* `image:packaging`: Provides an environment to build Debian packages.
  Depends on `image:ubuntu`.
* `image:runtime`: Provides an environment to run the Riju server and
  the test suite. Depends on `image:ubuntu`.
* `image:base`: Provides a base image upon which per-language images
  can be derived. Depends on `image:ubuntu`.
* `deb:lang-xyz`: For each language `xyz`, the Debian package that
  installs that language into the base image. Depends on
  `image:packaging` and the build script for the language (generated
  from the `install` clause of `langs/xyz.yaml`).
* `deb:shared-pqr`: Same but for shared dependencies, which are also
  archived as Debian packages.
* `image:lang-xyz`: For each language `xyz`, the per-language image
  used for user sessions in that language. Depends on `image:base`,
  `deb:lang-xyz`, and possibly one or more `deb:shared-pqr`.
* `test:lang-xyz`: An artifact certifying that the `xyz` language
  tests passed. Depends on `image:runtime`, `image:lang-xyz`, the test
  suite and API protocol code, and the `xyz` language configuration.
* `image:app`: Built on top of `image:runtime` but including the Riju
  server code, so that it can be run standalone. Depends on
  `image:runtime` and the application code.
* `deploy:ready`: Deployment configuration, ready to upload. Depends
  on `image:app` and all `test:lang-xyz`.
* `deploy:live`: Pseudo-artifact corresponding to actually running the
  deployment, which is a blue/green cutover in which all languages and
  the application server are updated at once.

### Depgraph abstractions

Each artifact has:

* A list of zero or more dependencies on other artifacts.
* A recipe to build it locally assuming that its dependencies are also
  available locally.
* A recipe to upload the local build artifact to a remote registry.
* A recipe to download the artifact from a remote registry to
  overwrite the local version.
* A way to compute a content-based hash of the artifact's dependencies
  and non-artifact inputs (data files and code). Crucially this does
  not require the dependencies to actually be built (the hash for each
  artifact is based only on its dependencies *hashes*), so it's
  possible to compute hashes for the entire dependency tree before
  doing anything.
* A way to check the hash currently attached to a local artifact,
  which can be compared to the desired hash to see if it needs to be
  rebuilt.
* A way to check the hash currently attached to an artifact in a
  remote registry, which can also be compared to the desired hash.

There are special types of artifacts:

* *Manual* artifacts do not have a hash until they are generated.
  Therefore, they must be built manually before the rest of the
  dependency calculations can proceed. `image:ubuntu` is a manual
  artifact since its hash depends on what we download from
  `ubuntu:rolling`.
* *Publish* artifacts do not have a hash after they are generated.
  Therefore, nothing can declare a dependency on them. `deploy:live`
  is a publish artifact. (Actually `deploy:ready` is a publish
  artifact too, but that is an implementation detail because I was
  lazy about my abstractions.)

### Usage of Depgraph

```
$ dep --help
Usage: dep <target>...

Options:
  --list         list available artifacts; ignore other arguments
  --manual       operate explicitly on manual artifacts
  --hold-manual  prefer local versions of manual artifacts
  --all          do not skip unneeded intermediate artifacts
  --local-only   do not fetch artifacts from remote registries
  --publish      publish artifacts to remote registries
  --yes          execute plan without confirmation
  -h, --help     display help for command
```

You can run `dep --list` to list all the available artifacts. Then
`dep name-of-artifact [names-of-more-artifacts...]` will generate
those artifacts. Depgraph is like Terraform in that it will compute a
plan and then ask you to confirm before proceeding.

By default Depgraph will generate artifacts locally only, although it
will download remote artifacts if appropriate versions exist in the
registry. Pass `--publish` to also cache generated artifacts in the
remote registries. Of course `--publish` is required to build
`deploy:live`.

For dealing with `image:ubuntu` specifically, you probably just want
to fetch Riju's version (available in a public ECR repository) using
`make sync-ubuntu` to keep in sync. However if you do want to update
to the latest `ubuntu:rolling`, it's `dep image:ubuntu --manual`.

The other options (aside from `--yes`) are mostly not too useful.
Depgraph is very sophisticated and should always compute the minimum
necessary build plan based on any changes you have made. So, you don't
need to worry about the details! (Except when the hashing isn't
working properly. Then you cry.)

## Makefile

To get a "quick" overview, run `make help`.

### Preliminary targets

There are a couple of targets that are independent of Depgraph and
need to be run just to make sure various bits of state and generated
files are up to date. Depgraph and/or `ci-run.bash` take care of this.

* `make ecr`: Authenticate to ECR, needed to push and pull. The
  authentication only lasts for 12 hours unfortunately, although it
  does survive an admin shell restart.
* `make all-scripts`: Generate packaging scripts (`build.bash` and
  `install.bash` in `build/{lang,shared}`) from YAML configuration.
* `make system`: Compile setuid binary used for spinning up and
  tearing down user containers. This is needed early because we use
  real containers in the test suite.

### Building Depgraph artifacts

First let's go through each of the Depgraph-enabled artifacts above.
For each one, there's:

* a way to build it locally
* a way to publish the local version to a remote registry
* a way to download the remote version locally

#### Docker images

Generally you build a Docker image named `image:foobar` using `make
image I=foobar`, you publish it with `make push I=foobar`, and you
download it with `make pull I=foobar`. Pass `NC=1` to `make image` to
disable Docker cache (although this is fairly rarely useful, and in
general for this to work with Depgraph we need a more sophisticated
mechanism).

There are one or two exceptions to this, unfortunately:

* For language images (`image:lang-foobar`), it's `make image I=lang
  L=foobar`.
* For `image:ubuntu`, you likely don't want to "build" it yourself
  (meaning take the latest `ubuntu:rolling` from Docker Hub). You can
  synchronize with upstream Riju using `make sync-ubuntu`.

For any Docker image `image:foobar`, you can jump into a shell using
`make shell I=foobar`. This has some optional arguments:

* `E=1`: Expose Riju ports outside the container. Most likely used as
  `make shell I=runtime E=1` inside the admin shell.
* `EE=1`: Same as `E=1`, but expose ports on `0.0.0.0` outside the
  container. This is helpful if you're running on the dev server and
  want to be able to access the development version of Riju in your
  browser.
* `CMD="make something"`: Instead of launching an interactive Bash
  shell inside the container, run the specified shell command (using
  Bash) and exit.

Riju source code and build directories are typically cross-mounted at
`/src` inside all non-user containers, so there is generally no need
to rebuild and/or restart containers when making changes.

Note that all of this section applies also to the `admin` and `ci`
images, which are not otherwise involved with Depgraph (and are based
directly on upstream `ubuntu:rolling`).

Note also that `admin` uses `--network=host` and maps a number of
directories such as `~/.ssh` and `~/.aws`, plus the Docker socket,
inside the container, so you can treat an admin shell more or less the
same as your external development environment.

#### Debian packages

Build a language package using `make pkg T=lang L=xyz` (where there
exists `langs/xyz.yaml`). Build a shared dependency package using
`make pkg T=shared L=pqr` (where there exists `shared/pqr.yaml`).

This has to be done in the packaging image, and will abort otherwise.
So, for short, `make shell I=packaging CMD="make pkg T=lang L=xyz"`.

To debug package installation, you can jump into a persistent
packaging shell (`make shell I=packaging`) and break down the process
into three steps:

* `make pkg-clean T=lang L=xyz`: Delete and recreate packaging
  directories for `xyz`.
* `make pkg-build T=lang L=xyz`: Run the packaging script. Substitute
  `build` for `debug` to instead start a shell in the environment of
  the packaging script, where you can operate manually.
* `make pkg-deb T=lang L=xyz`: Compress the results of the packaging
  script into a file `build/lang/xyz/riju-lang-xyz.deb`. You can pass
  `Z=(gzip|xz)` to enable compression, which is disabled by default to
  save on time during development. Otherwise, packages are
  automatically recompressed before registry upload time.

Uploading a package to the registry is `make upload T=lang L=xyz`, and
download is `make download T=lang L=xyz`.

#### Tests

You can run tests for a specific language (inside the `runtime` image
only, otherwise it will abort) using `make test L=xyz`. `L` can also
be a comma-separated list of languages. You can additionally (or
instead) filter by test type, e.g. `make test L=python T=lsp`.
Uploading and downloading test hashes is only implemented at the
Depgraph layer.

#### Final deployment

* `make deploy-config`: Build the deployment configuration JSON that
  will be pushed to S3.
* `make deploy-latest`: Push it to S3.
* `make deploy`: Combination of the above.

### Application build

We have three compiled parts of Riju:

* Frontend assets (compiled with Webpack)
* Setuid binary used for privilege deescalation (compiled with LLVM)
* Supervisor binary used on deployed images (compiled with Go tooling)

For development:

* `make frontend-dev` (compile frontend assets, auto recompile on
  change)
* `make system-dev` (compile setuid binary, auto recompile on change)
* `make supervisor-dev` (compile supervisor binary, auto recompile on
  change)
* `make server-dev` (run server, auto restart on change)
* `make dev` (all four of the above)

For production:

* `make frontend` (compile frontend assets)
* `make system` (compile setuid binary)
* `make supervisor` (compile supervisor binary)
* `make build` (all three of the above)
* `make server` (run server)

### Miscellaneous utilities

* `make sandbox`: Bash shell emulating a user session at the command
  line, with many useful functions in scope for executing various
  commands from the language configuration YAML.
* `make lsp`: LSP REPL. This is not working currently as it needs to
  be updated for the new build system that uses per-language images.
* `make dockerignore`: Update `.dockerignore` from `.gitignore`.
* `make tmux`: Start a tmux session conveniently with variables from
  `.env`.
* `make env`: Load in `.env` environment variables in a subshell. You
  can also do this for your current session (though it won't affect
  new tmux panes) with `set -a; . .env; set +a`.

### Infrastructure

There are wrappers for `packer` and `terraform` in the repository
`bin` that deal with some niceties automatically. Run `make packer` to
do an AMI build, and use `terraform` commands from any directory.
