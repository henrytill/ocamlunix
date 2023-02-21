{ pkgs ? import <nixpkgs> {}
, compiler ? "4.14.1"
}:

let
  lib = pkgs.lib;
  opam-nix = import (builtins.fetchGit {
    url = "https://github.com/tweag/opam-nix.git";
    rev = "15f724c3e01066fdb41f9ae7e3123f26a65b1d33";
  });
  on = opam-nix.lib.${builtins.currentSystem};
  args = {
    inherit pkgs;
    resolveArgs.with-test = true;
  };
  src = lib.cleanSource ./.;
  query = {
    ocaml-base-compiler = compiler;
  };
  scope = on.buildOpamProject args "ocamlunix" src query;
in {
  ocamlunix = scope.ocamlunix;
}
