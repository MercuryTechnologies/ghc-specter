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
    fficxx = {
      url = "github:wavewave/fficxx/aarch64-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    hs-ogdf = {
      url = "github:wavewave/hs-ogdf/aarch64-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.fficxx.follows = "fficxx";
    };
  };
  outputs = { self, nixpkgs, flake-utils, concur, concur-replica, replica
    , fficxx, hs-ogdf }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlayGHC = final: prev: {
          haskellPackages = prev.haskell.packages.ghc923;
        };
        pkgs = import nixpkgs {
          overlays = [
            overlayGHC
            (fficxx.overlay.${system})
            (hs-ogdf.overlay.${system})
          ];
          inherit system;
          config.allowBroken = true;
        };
        hspkgs = pkgs.haskellPackages.override (old: {
          overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
            (self: super: {
              "concur-core" =
                self.callCabal2nix "concur-core" (concur + "/concur-core") { };
              "concur-replica" =
                self.callCabal2nix "concur-replica" concur-replica { };
              "replica" = self.callCabal2nix "replica" replica { };
              "retry" = pkgs.haskell.lib.dontCheck super.retry;
              "PyF" = pkgs.haskell.lib.dontCheck super.PyF;
            });
        });
        hsenv = hspkgs.ghcWithPackages (p: [
          p.aeson
          p.binary-instances
          p.cabal-install
          p.concur-core
          p.concur-replica
          p.discrimination
          p.extra
          p.fourmolu
          p.hiedb
          p.hpack
          p.hspec
          p.OGDF
          p.optparse-applicative
          p.pretty-simple
          p.PyF
          p.replica
          p.socket
          p.text
          p.time
        ]);
      in {
        devShells.default = pkgs.mkShell { packages = [ hsenv pkgs.nixfmt ]; };
      });
}
