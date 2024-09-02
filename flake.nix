{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs.opam-repository.follows = "opam-repository";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
      follows = "opam-nix/flake-utils";
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs =
    {
      self,
      flake-utils,
      opam-nix,
      nixpkgs,
      ...
    }@inputs:
    let
      package = "ocamlunix";
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};
        args = {
          resolveArgs.with-test = true;
        };
        src = builtins.path {
          path = ./.;
          name = "ocamlunix-src";
        };
        query = {
          ocaml-base-compiler = "4.14.1";
        };
        scope = on.buildDuneProject args package src query;
        overlay = final: prev: { };
      in
      {
        legacyPackages = scope.overrideScope' overlay;
        packages.default = self.legacyPackages.${system}.${package};
      }
    );
}
