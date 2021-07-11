# Tutorial: install your language

Most likely, your language isn't installed by default in Ubuntu. If
not, you'll need to add an `install:` block to your language
configuration describing how to install it.

The easiest case is system packages, which are supported out of the
box, for example:

```yaml
install:
  apt:
    - ruby
  npm:
    - pug-cli
  pip:
    - bython
  gem:
    - solargraph
  cpan:
    - Acme::Chef
  opam:
    - ocamlformat
```

For more advanced configuration you need to understand how Debian
packaging works. To build a package, we place files into a `${pkg}`
directory mirroring how they should be installed on the system, so for
example a binary intended for `/usr/local/bin/prettier` would be
placed at `${pkg}/usr/local/bin/prettier`. Then the `${pkg}` directory
can be turned into a `.deb`, which can be installed on any system.

**If you have trouble, see the tutorial on [debugging package
installation](install-debugging.md).**

## Download a binary or script

We prefer to put all binaries in `/usr/local/bin` rather than
`/usr/bin` where they might conflict with default Ubuntu packages.

```yaml
install:
  manual: |
    install -d "${pkg}/usr/local/bin"

    wget http://www.blue.sky.or.jp/grass/grass.rb
    chmod +x grass.rb
    cp -T grass.rb "${pkg}/usr/local/bin/grass"
```

## Get the latest version from GitHub

There is a predefined `latest_release` function that can be used to
get the name of the latest tag in a GitHub repository.

```yaml
install:
  manual: |
    install -d "${pkg}/usr/local/bin"

    ver="$(latest_release snoe/clojure-lsp)"
    wget "https://github.com/snoe/clojure-lsp/releases/download/${ver}/clojure-lsp"
    chmod +x clojure-lsp
    cp clojure-lsp "${pkg}/usr/local/bin/"
```

## Get the latest version from some other website

In the case that there's no "download the latest version" URL, we
typically use disgusting `grep` pipelines to extract it in a
semi-reliable way from a stable-seeming webpage.

```yaml
install:
  manual: |
    install -d "${pkg}/usr/local/bin"

    path="$(curl -fsSL https://static.red-lang.org/download.html | grep -Eo '/dl/linux/[^"]+' | head -n1)"
    wget "https://static.red-lang.org${path}" -O red
    chmod +x red
    cp red "${pkg}/usr/local/bin/"
```

## Unpack a tarball

Sometimes tarballs can be extracted directly to `/usr/local`. However,
unless there's an obviously correct way to do that, we typically put
language distributions in `/opt/<lang>` and then put symlinks in
`/usr/local/bin` for the binaries. The `-C` and `--strip-components`
flags are very helpful for controlling the extraction.

Also, one thing that might be unintuitive is that we use system-global
paths for the targets of symlinks (`/opt/swift/bin/swiftc`) but
`${pkg}` paths for the link names (`${pkg}/usr/local/bin/swiftc`).
This is because while we are putting all files into `${pkg}` during
build, the eventual place they will be installed by the package is
into the root filesystem, so any references to paths *within* files
(including symlink targets) must not mention `${pkg}`. This is a
standard feature of all Linux packaging tools.

```yaml
install:
  manual: |
    install -d "${pkg}/opt/swift"
    install -d "${pkg}/usr/local/bin"

    ver="$(latest_release apple/swift | grep -Eo '[0-9.]+')"
    wget "https://swift.org/builds/swift-${ver}-release/ubuntu2004/swift-${ver}-RELEASE/swift-${ver}-RELEASE-ubuntu20.04.tar.gz" -O swift.tar.gz
    tar -xf swift.tar.gz -C "${pkg}/opt/swift" --strip-components=2
    ln -s /opt/swift/bin/swiftc /opt/swift/bin/sourcekit-lsp "${pkg}/usr/local/bin/"
```

## Compile from source

Sometimes there is no binary distribution. In this case you want to
clone/download the source code and run the appropriate compilation
command. GCC and LLVM plus a number of scripting languages are already
installed into the `packaging` image by default, but you might have to
include a `prepare` block listing additional things to install to be
able to compile the software.

For example, in this case the `build.sh` script provided by Zot
happens to require some Qt development dependencies at compile time,
and a non-development dependency at runtime.

```yaml
install:
  prepare:
    apt:
      - qt5-qmake
      - qtscript5-dev
  apt:
    - libqt5script5
  manual: |
    install -d "${pkg}/usr/local/bin"

    git clone https://github.com/manyoso/zot.git
    pushd zot
    ./build.sh
    cp build/bin/zot "${pkg}/usr/local/bin/"
    popd
```

## Use a shared dependency

Your language may need to use software such as Prettier which is also
used by other languages but does not have an Ubuntu package. In that
case we package it as a Riju shared dependency (check the `shared`
directory for a list), and you declare the dependency using the `riju`
key.

```yaml
install:
  riju:
    - prettier
```

## Install custom scripts or config files

Sometimes you may need to add a wrapper script or config file
somewhere on the filesystem. Riju has the `scripts` and `files` keys
for this. The keys of these maps are the filesystem paths (`scripts`
defaults to putting things in `/usr/local/bin` if relative paths are
given).

```yaml
install:
  scripts:
    teco-encode: |
      #!/usr/bin/env -S python3 -u

      import re
      import sys

      for line in sys.stdin:
          line = re.sub(r"\^(.)", lambda m: chr(ord(m.group(1)) ^ 0b1000000), line)
          line = line.replace("$", chr(27))
          print(line, end="")
  files:
    "/opt/sqlite/sqls.yml": |
      connections:
        - driver: sqlite3
          dataSourceName: db.sqlite3
```

## Setting up skeleton files

Sometimes your language may require some configuration files in
addition to the actual file to be compiled. Or alternatively, your
language may do a bunch of caching work the first time it starts up,
which you don't want to happen every time someone launches it on Riju.
Both of these problems can be solved by preparing files to be copied
into the user's home directory at the start of a new Riju session
(this is done using the `setup` key, which is explained later).

In the following example, we ask .NET to create a project template for
us, and then compile the project. Then we take the files that were
created for the project, plus cache directories in `$HOME`, and copy
them into `/opt` so that they can be copied back later using `setup`.
By convention, we name such directories `skel`.

```yaml
install:
  prepare:
    apt:
      - $(grep-aptavail -wF Package "dotnet-sdk-3\.[0-9.]+" -s Package -n | sort -Vr | head -n1)
  manual: |
    install -d "${pkg}/opt/qsharp/skel"

    dotnet new -i Microsoft.Quantum.ProjectTemplates
    dotnet new console -lang Q# -o main
    dotnet run --project main

    shopt -s dotglob
    cp -R * "${HOME}/.dotnet" "${HOME}/.nuget" "${pkg}/opt/qsharp/skel/"
    rm "${pkg}/opt/qsharp/skel/main/Program.qs"
    chmod -R a=u,go-w "${pkg}/opt/qsharp/skel"
```

## Dealing with versioned APT packages

For some reason, some APT packages have version numbers in their
names. For example, you can't `sudo apt install lua`; you have to
`sudo apt install lua5.4`. The best way to deal with these situations
is to use various `grep-aptavail` hacks to identify the latest
available version programmatically. Check the man page as well as uses
of `grep-aptavail` in Riju to understand the options.

```yaml
install:
  apt:
    - $(grep-aptavail -XF Provides lua -a -XF Version "$(grep-aptavail -XF Provides lua -s Version -n | sort -Vr | head -n1)" -s Package -n | head -n1)
```
