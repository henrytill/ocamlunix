{ pkgs ? import <nixpkgs> {}
, system ? builtins.currentSystem
, compiler ? "4.14.1"
}:

let
  opam-nix = import (builtins.fetchGit {
    url = "https://github.com/tweag/opam-nix.git";
    rev = "15f724c3e01066fdb41f9ae7e3123f26a65b1d33";
  });
  on = opam-nix.lib.${system};
  args = {
    inherit pkgs;
    resolveArgs.with-test = true;
  };
  src = pkgs.lib.cleanSource ./.;
  query = {
    ocaml-base-compiler = compiler;
  };
  scope = on.buildOpamProject args "ocamlunix" src query;
in {
  ocamlunix = scope.ocamlunix;
}
