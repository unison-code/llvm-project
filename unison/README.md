# Getting Started with Unison

Unison is a simple, flexible, and potentially optimal software tool that
performs register allocation and instruction scheduling in integration using
combinatorial optimization.

## Prerequisites

Unison has the following dependencies:
[Stack](http://www.haskellstack.org/),
[Qt](https://www.qt.io/) (version 5.x, optional see [#33](https://github.com/unison-code/unison/issues/33)),
[Graphviz library](http://www.graphviz.org/) (also optional), and
[Gecode](http://www.gecode.org/) (version 6.0.0).
To get the first three dependencies in Debian-based distributions, just run:

```
apt-get install haskell-stack qtbase5-dev libgraphviz-dev
```

Upgrade Slack after installing it:

```
stack upgrade
```

The source of Gecode can be fetched with:

```
wget https://github.com/Gecode/gecode/archive/release-6.0.0.tar.gz
```

## Building

Just go to the `src` directory and run:

```
make build
```

## Testing

Unison contains a test suite with a few functions where different targets and
optimization goals are exercised. To execute the tests just run:

```
make test
```

## Installing

The building process generates three binaries. The installation process consists
in copying the binaries into the appropriate system directory. To install the
binaries under the default directory `usr/local` just run:

```
make install
```

The installation directory is specified by the Makefile variable `PREFIX`. To
install the binaries under an alternative directory `$DIR` just run:

```
make install PREFIX=$DIR
```

## Running

Unison can be run as a standalone tool but is only really useful as a complement
to a full-fledged compiler such as [LLVM](http://llvm.org/). [Our LLVM
fork](https://github.com/unison-code/llvm) includes a Unison driver built on top
of LLVM's `llc` code generator. To try it out, just clone the LLVM fork and
follow the instructions in the `README.md` file from any of the branches with a
`-unison` suffix.

## Documentation

Check out Unison's work-in-progress
[manual](https://unison-code.github.io/doc/manual.pdf). The manual's source can
be found in the `doc` directory.

Source-level documentation is also available for the core Haskell modules of
Unison (`MachineIR.Base`, `Unison.Base`, and `Unison.Target.API`). To generate
this documentation in HTML format and open it with a web browser, just run:

```
make doc
```

## Contact

[Roberto Castañeda Lozano](https://robcasloz.github.io/) [<rcas@acm.org>]

## License

Unison is licensed under the BSD3 license, see the [LICENSE.md](LICENSE.md) file
for details.

## Further Reading

Check [the Unison website](https://unison-code.github.io/).
