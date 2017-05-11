bitset — Library for manipulating sets from defined universe.
-------------------------------------------------------------------------------
%%VERSION%%

bitset is TODO

bitset is distributed under the ISC license.

Homepage: https://github.com/vasil-sd/ocaml-bitset  

## Installation

bitset can be installed with `opam`:

    opam install ocaml-bitset

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is generated from the source
interfaces. It can be consulted [online][doc] or via `odig doc
bitset`.

[doc]: https://vasil-sd.github.io/ocaml-bitset/doc

## Sample programs

If you installed bitset with `opam` sample programs are located in
the directory `opam var bitset:doc`.

In the distribution sample programs and tests are located in the
[`test`](test) directory. They can be built and run
with:

    topkg build --tests true && topkg test 
