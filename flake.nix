{
  description = "ghc-build-analzyer: utility for analyzing GHC build";
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "avieth";
      repo = "nixpkgs";
      rev = "346134b9b036a6f5c3f7239b450343c88954acea";
    };
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
          haskellPackages = prev.haskell.packages.ghc924;
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
              "criterion" = pkgs.haskell.lib.dontCheck super.criterion;
              "concur-core" =
                self.callCabal2nix "concur-core" (concur + "/concur-core") { };
              "concur-replica" =
                self.callCabal2nix "concur-replica" concur-replica { };
              "http2" = pkgs.haskell.lib.dontCheck super.http2;
              "replica" = self.callCabal2nix "replica" replica { };
              "retry" = pkgs.haskell.lib.dontCheck super.retry;
            });
        });
        hsenv = hspkgs.ghcWithPackages (p: [
          p.aeson
          p.binary-instances
          p.cabal-install
          p.concur-core
          p.concur-replica
          p.conduit
          p.conduit-extra
          p.discrimination
          p.extra
          p.fourmolu
          p.hiedb
          p.hpack
          p.hspec
          p.lens
          p.OGDF
          p.optparse-applicative
          p.replica
          p.socket
          p.text
          p.time
        ]);
      in {
        devShells.default = pkgs.mkShell { packages = [ hsenv pkgs.nixfmt ]; };
      });
}
