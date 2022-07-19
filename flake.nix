{
  description = "ghc-build-analzyer: utility for analyzing GHC build";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
    concur = {
      url = "github:wavewave/concur/ghc-9.2";
      flake = false;
    };
    concur-replica = {
      url = "github:wavewave/concur-replica/ghc-9.2";
      flake = false;
    };
    replica = {
      url = "github:wavewave/replica/ghc-9.2";
      flake = false;
    };
  };
  outputs = { self, nixpkgs, flake-utils, concur, concur-replica, replica }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowBroken = true;
        };
        hspkgs = pkgs.haskell.packages.ghc923.override (old: {
          overrides = self: super: {
            "concur-core" =
              self.callCabal2nix "concur-core" (concur + "/concur-core") { };
            "concur-replica" =
              self.callCabal2nix "concur-replica" concur-replica { };
            "replica" = self.callCabal2nix "replica" replica { };
            "retry" = pkgs.haskell.lib.dontCheck super.retry;
          };
        });
        hsenv = hspkgs.ghcWithPackages (p: [
          p.aeson
          p.cabal-install
          p.concur-core
          p.concur-replica
          p.fourmolu
          p.hpack
          p.optparse-applicative
          p.replica
          p.text
        ]);
      in {
        devShells.default = pkgs.mkShell { packages = [ hsenv pkgs.nixfmt ]; };
      });
}
