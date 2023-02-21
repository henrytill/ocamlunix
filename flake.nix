{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    nixpkgs.follows = "opam-nix/nixpkgs";
    flake-utils = {
      url = "github:numtide/flake-utils";
      follows = "opam-nix/flake-utils";
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
      follows = "opam-nix/flake-compat";
    };
  };

  outputs = { self, flake-utils, opam-nix, nixpkgs, ... }@inputs:
    let
      package = "ocamlunix";
      duneFilter = path: type: type != "directory" || baseNameOf path != "_build";
      opamFilter = path: type: type != "directory" || baseNameOf path != "_opam";
    in
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};
        processSource = input:
          with pkgs.lib; cleanSourceWith {
            filter = duneFilter;
            src = cleanSourceWith {
              filter = opamFilter;
              src = cleanSource input;
            };
          };
        args = {
          resolveArgs.with-test = true;
        };
        src = processSource ./.;
        query = { ocaml-base-compiler = "4.14.1"; };
        scope = on.buildDuneProject args package src query;
        overlay = final: prev: {};
      in {
        legacyPackages = scope.overrideScope' overlay;
        packages.default = self.legacyPackages.${system}.${package};
      });
}
