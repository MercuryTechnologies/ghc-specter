{
  description = "ghc-build-analzyer: utility for analyzing GHC build";
  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/master"; # d142881f4318260f56f9ff8baedd9197c712024f;
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem flake-utils.lib.allSystems (system:
      let
        pkgs = import nixpkgs { inherit system; };
        hspkgs = pkgs.haskell.packages.ghc923;
        hsenv = hspkgs.ghcWithPackages (p: [
          p.aeson
          p.cabal-install
          p.hpack
          p.optparse-applicative
          p.text
        ]);
      in { devShell = pkgs.mkShell { buildInputs = [ hsenv pkgs.nixfmt ]; }; });
}
