#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "bitset" @@ fun c ->
  Ok [ Pkg.mllib "src/bitset.mllib";
       Pkg.test "test/test"; ]
