{
  description = "ghc-build-analzyer: utility for analyzing GHC build";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
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
      in {
        devShells.default = pkgs.mkShell { packages = [ hsenv pkgs.nixfmt ]; };
      });
}
