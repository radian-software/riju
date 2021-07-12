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

To get a quick overview, run `make help`.

## Build artifacts

We have two kinds of artifacts: Docker images (`I=` in the Makefile)
and Debian packages (`L=` and `T=` in the Makefile).

### Docker images

* `admin`: The first thing you build, and then everything else
  (including building other Docker images) is done from inside.
* `ci`: Same as `admin` but for CI, so it has only the minimum number
  of dependencies.
* `packaging`: Provides an environment to build Debian packages.
* `runtime`: Base runtime environment for Riju into which Debian
  packages are installed and in which the server is expected to run.
* `composite`: Based on `runtime`, but with all languages' Debian
  packages installed.
* `compile`: Compiles the Riju application code (i.e. everything
  that's not per-language).
* `app`: Based on `composite`, but with compiled code copied over from
  `compile`. This container serves traffic in production.

Docker images are built by running `make image I=<image> [NC=1]`, and
run by `make shell I=<image> [E=1]`.

Riju source code and build directories are typically mounted at `/src`
inside the container, so there is generally no need to rebuild and/or
restart containers when making changes. (Exception: `compile` and
`app`.)

* `NC=1`: pass `--no-cache` to `docker build`. Note that caching is
  always disabled for `composite` due to the unique way in which the
  build process is implemented for that image (to ensure good
  performance).
* `E=1`: map ports to the host. Generally desired for `runtime`, not
  needed for `admin`.

Note that `admin` uses `--network=host` and maps a number of
directories such as `~/.ssh` and `~/.aws`, plus the Docker socket,
inside the container, so you can treat an admin shell more or less the
same as your external development environment.

Note also that Docker builds do not pull new base images. For that,
use `make pull-base`.

### Debian packages

There are three types of Debian packages:

* `lang`, e.g. `riju-lang-python` (`T=lang L=python`): Installs the
  actual language and any associated tools. May declare dependencies
  on other Ubuntu packages, and may include files directly.
* `config`, e.g. `riju-config-python` (`T=config L=python`): Installs
  a JSON configuration file into `/opt/riju/langs`. The server looks
  in this directory to find which languages are supported.
* `shared`, e.g. `riju-shared-pandoc` (`T=shared L=pandoc`): Shared
  dependency. This is for when multiple different languages need the
  same tool, and there's no Ubuntu package for it.

There are three basic actions for any particular Debian package:

* From any container, run `make script L=<lang> T=<type>` to generate
  the build script for a package. This is placed in
  `build/<type>/<lang>/build.bash`.
* From a packaging container, run `make pkg L=<lang> T=<type>` to
  (re)build a Debian package by executing its build script in a fresh
  directory. This is placed in
  `build/<type>/<lang>/riju-<type>-<lang>.deb`.
* From a runtime container, run `make install L=<lang> T=<type>` to
  install it.

Each language consists of a `lang` and `config` package, so you need
to follow the above steps for both. The `make scripts L=<lang>`, `make
pkgs L=<lang>`, and `make installs L=<lang>` commands automate this.

For further convenience, if you already have a runtime container up,
from the admin shell you can use `make repkg L=<lang> T=<type>` and/or
`make repkgs L=<lang>` to automate the three steps above (run `make
script`, run `make pkg` inside a fresh packaging container, and then
run `make install` inside the existing runtime container).

Some `lang` packages declare `shared` dependencies, in which case they
won't install until the `shared` package is built and installed
already. This can't be done with `make scripts`, `make pkgs`, `make
installs`, or `make repkgs`: use `make script T=shared L=<lang>`,
`make pkg T=shared L=<lang>`, `make install T=shared L=<lang>`, or
`make repkg T=shared L=<lang>`, respectively. (Check the
`install.riju` key in a language's YAML configuration to see if it
declares any such dependencies.)

#### Package build details

The build script is executed with a working directory of
`build/<type>/<lang>/src`, and it installs package files into
`build/<type>/<lang>/pkg`.

If `make pkg` is too high-level, there are more specific commands:

* `make pkg-clean`: Wipe and recreate the `src` and `pkg` directories.
* `make pkg-build`: Just run the package build script (you also need
  to run `make script` if the language configuration has changed).
* `make pkg-deb`: Build the `pkg` directory into the actual Debian
  package.

All Makefile targets with `pkg` in the name take an optional `Z`
parameter for the `.deb` compression level, defaulting to `none`. This
can be increased to `gzip` or even further to `xz`. Increasing the
compression massively increases build time, but massively decreases
the resulting package size.

## Artifact caching

All artifacts can be cached on remote registries to avoid being
rebuilt in CI unnecessarily.

* Docker images are cached on Docker Hub. Push with `make push
  I=<image>` and pull with `make pull I=<image>`.
* Debian packages are cached on S3. Push with `make upload T=<type>
  L=<lang>` and pull with `make download T=<type> L=<lang>`.

CI will take care of managing the remote registries automatically. It
is generally recommended to let CI handle this, and not push anything
yourself.

## Application build

We have two compiled parts of Riju:

* Frontend assets (compiled with Webpack)
* Setuid binary used for privilege deescalation (compiled with LLVM)

For development:

* `make frontend-dev` (compile frontend assets, auto recompile on
  change)
* `make system-dev` (compile setuid binary, auto recompile on change)
* `make server-dev` (run server, auto restart on change)
* `make dev` (all three of the above)

For production:

* `make frontend` (compile frontend assets)
* `make system` (compile setuid binary)
* `make build` (both of the above)
* `make server` (run server)

## Incremental builds and hashing

CI is set up so that artifacts are only rebuilt when changes have
occurred. This is done through an extensive hashing algorithm which
produces a consistent hash for each artifact based on its inputs. We
can then check whether the hash has changed, meaning the artifact
should be rebuilt.

This is implemented mostly behind the scenes, but you can run `make
plan` to execute the hashing algorithm and dump a plan of the minimal
set of actions that would be run if this were CI:

* If local artifact is missing, but remote artifact is up to date:
  download remote to local.
* If remote artifact is missing or outdated, but local artifact is up
  to date: upload local to remote.
* If neither local nor remote artifact is up to date: rebuild local
  and upload to remote.

You can run `make sync` to execute this plan, excepting the upload
part (that should, for safety, generally be done only in CI). So, in
principle, `make sync` should bring all your local artifacts up to
date with the latest source (rebuilding some if needed).

To run a full deployment, use `make publish`. This should definitely
be done only from CI, and with the `Z=xz` flag to enable Debian
package compression.
