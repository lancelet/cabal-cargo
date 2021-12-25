# cabal-cargo

[![GitHub CI](https://github.com/lancelet/cabal-cargo/workflows/CI/badge.svg)](https://github.com/lancelet/cabal-cargo/actions)
[![Hackage](https://img.shields.io/hackage/v/cabal-cargo.svg?logo=haskell)](https://hackage.haskell.org/package/cabal-cargo)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

## What is this Monstrosity?

Haskell packages can include C code, using
[`c-sources`](https://cabal.readthedocs.io/en/3.4/cabal-package.html#pkg-field-c-sources)
in Cabal. Imagine if, instead of C, you could use Rust in the same way. Enter
`cabal-cargo`!

Before you get too excited: it's not completely working yet. The approach used
by `cabal-cargo` seems to work fine for macOS and Linux, but there are some
linking issues under Windows that I'm still working through.

## Outline: How does it kind-of work?

The basic idea of `cabal-cargo` is to defer to
[`cargo`](https://doc.rust-lang.org/cargo/) to manage Rust dependencies and
compilation. It uses a standard Rust project structure to build a [`staticlib`
crate](https://doc.rust-lang.org/cargo/reference/cargo-targets.html#the-crate-type-field).
The interface from Haskell to Rust is via the standard C API. I recommend that
the C interface be managed by [`cbindgen`](https://github.com/eqrion/cbindgen),
which generates a C header file from the Rust library. This header file can be
used by the modern [`capi` FFI
bindings](https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/ffi.html#ffi-capi)
in Haskell.

`cabal-cargo` provides a function to be used in a custom `Setup.hs` file. Cabal
hooks then do the following:

  1. Compile the Rust source when `cabal build` is invoked.

  1. Make the Rust library accessible to the Cabal project.
  
  1. Make the Rust C header file accessible to the Cabal project.

## Why do you want Cabal to call Cargo to build the Rust code?

An alternative approach is of course to compile the Rust library independently
and link to it as though it were a regular C external library. The Haskell
bindings for SDL2 work this way, for example. However, such an "external
library" approach means that users of your library must first install the Rust
library, which is inconvenient if you want to include a lot of Rust code. This
is particularly inconvenient if the only use of the Rust code is to provide
functionality for your Haskell project. Having Cabal call Cargo to compile the
Rust code means that a user only needs to have the Rust toolchain installed, and
then Cabal can manage everything else. In the small Haskell development
community, I think it helps to reduce the number of steps that users of
libraries have to take.

## How do I use it in my own project?

It's currently experimental. But take a look at the `test-project` directory and
go for it!

## "I have a better way to do all this; your approach is silly!"

Go for it! I'd like a good way to integrate Rust with my Haskell projects, and I
don't care one whit if it's not my own approach. Start exploring, and please
ping me so that I can link to other alternative projects from this README.
