{
  description = "ghc-build-analzyer: utility for analyzing GHC build";
  inputs = {
    # nixpkgs/master on 2022-10-21
    nixpkgs.url =
      "github:NixOS/nixpkgs/71c5816834f93840dd301ec384c9d7947e97c27d";
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
      url = "github:wavewave/fficxx/master";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    hs-ogdf = {
      url = "github:wavewave/hs-ogdf/master";
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
          #overlays = [
          #  overlayGHC
          #  (fficxx.overlay.${system})
          #  (hs-ogdf.overlay.${system})
          #];
          inherit system;
          #config.allowBroken = true;
        };

        haskellOverlay = final: hself: hsuper: {
          "criterion" = final.haskell.lib.dontCheck hsuper.criterion;
          "concur-core" =
            hself.callCabal2nix "concur-core" (concur + "/concur-core") { };
          "concur-replica" =
            hself.callCabal2nix "concur-replica" concur-replica { };
          "http2" = final.haskell.lib.dontCheck hsuper.http2;
          "replica" = hself.callCabal2nix "replica" replica { };
          "retry" = final.haskell.lib.dontCheck hsuper.retry;
        };

        hpkgsFor = compiler:
          pkgs.haskell.packages.${compiler}.extend (hself: hsuper:
            (fficxx.haskellOverlay.${system} pkgs hself hsuper)
            // (hs-ogdf.haskellOverlay.${system} pkgs hself hsuper)
            // (haskellOverlay pkgs hself hsuper));

        mkShellFor = compiler:
          let
            hsenv = (hpkgsFor compiler).ghcWithPackages (p: [
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
              p.lens
              p.OGDF
              p.optparse-applicative
              p.replica
              p.socket
              p.text
              p.time
            ]);
          in pkgs.mkShell { packages = [ hsenv pkgs.nixfmt ]; };

        supportedCompilers = [ "ghc924" "ghc942" ];
      in {
        inherit haskellOverlay;
        devShells = pkgs.lib.genAttrs supportedCompilers mkShellFor;
      });
}
