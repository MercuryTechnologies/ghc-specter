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
    hs-ogdf = {
      url = "github:wavewave/hs-ogdf/master";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs = { self, nixpkgs, flake-utils, concur, concur-replica, replica, hs-ogdf }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowBroken = true;
        };

        haskellOverlay = final: hself: hsuper: {
          "criterion" = final.haskell.lib.dontCheck hsuper.criterion;
          "concur-core" =
            hself.callCabal2nix "concur-core" (concur + "/concur-core") { };
          "concur-replica" =
            hself.callCabal2nix "concur-replica" concur-replica { };
          "discrimination" = hself.callHackage "discrimination" "0.5" { };
          "http2" = final.haskell.lib.dontCheck hsuper.http2;
          "newtype-generics" =
            final.haskell.lib.doJailbreak hsuper.newtype-generics;
          "replica" = hself.callCabal2nix "replica" replica { };
          "retry" = final.haskell.lib.dontCheck hsuper.retry;

          # fficxx-related
          "fficxx" =
            hself.callHackage "fficxx" "0.7.0.0" { };
          "fficxx-runtime" =
            hself.callHackage "fficxx-runtime" "0.7.0.0" { };
          "stdcxx" =
            hself.callHackage "stdcxx" "0.7.0.0" { };
          "template" =
            final.haskell.lib.doJailbreak hsuper.template;

          # TODO: check whether this will be fixed in GHC 9.4.3.
          "conduit-extra" = final.haskell.lib.dontCheck hsuper.conduit-extra;
          "file-embed" =
            hself.callHackage "file-embed" "0.0.15.0" { };
          # fixity-th is disabled to avoid segfault during compilation.
          "fourmolu" = final.haskell.lib.dontCheck
            (final.haskell.lib.overrideCabal
              (hself.callHackage "fourmolu" "0.10.1.0" { }) (drv: {
                configureFlags = [ "-f-fixity-th" ];
                libraryHaskellDepends = drv.libraryHaskellDepends
                  ++ [ hself.file-embed ];
              }));

          # ghc-debug related deps
          "bitwise" = final.haskell.lib.doJailbreak hsuper.bitwise;
          "brick" = hsuper.brick_1_3;
          "eventlog2html" =
            final.haskell.lib.doJailbreak (hself.callHackage "eventlog2html" "0.9.2" { });
          "ghc-events" = final.haskell.lib.doJailbreak hsuper.ghc-events;
          "monoidal-containers" =
            final.haskell.lib.doJailbreak hsuper.monoidal-containers;
          "microlens" =
            hself.callHackage "microlens" "0.4.13.1" { };
          "microlens-ghc" =
            hself.callHackage "microlens-ghc" "0.4.14.1" { };
          "microlens-platform" =
            hself.callHackage "microlens-platform" "0.4.3.3" { };
          "string-qq" = final.haskell.lib.doJailbreak hsuper.string-qq;
          "vty" = hself.callHackage "vty" "5.37" { };

          # ghc-debug-*
          "ghc-debug-common" =
            hself.callHackage "ghc-debug-common" "0.4.0.0" { };
          "ghc-debug-stub" =
            final.haskell.lib.doJailbreak (hself.callHackage "ghc-debug-stub" "0.4.0.0" { });
          "ghc-debug-client" =
            final.haskell.lib.doJailbreak (hself.callHackage "ghc-debug-client" "0.4.0.0" { });
          "ghc-debug-brick" =
            hself.callHackage "ghc-debug-brick" "0.4.0.0" { };
          "ghc-debug-convention" =
            hself.callHackage "ghc-debug-convention" "0.4.0.0" { };

          # ghc-specter-*
          "ghc-specter-plugin" =
            hself.callCabal2nix "ghc-specter-plugin" ./plugin { };
          "ghc-specter-daemon" =
            hself.callCabal2nix "ghc-specter-daemon" ./daemon { };
          "ghc-build-analyzer" =
            hself.callCabal2nix "ghc-build-analyzer" ./ghc-build-analyzer { };
        };

        hpkgsFor = compiler:
          pkgs.haskell.packages.${compiler}.extend (hself: hsuper:
            (hs-ogdf.haskellOverlay.${system} pkgs hself hsuper)
            // (haskellOverlay pkgs hself hsuper));

        mkShellFor = compiler:
          let
            hsenv = (hpkgsFor compiler).ghcWithPackages (p:
              [
                p.aeson
                p.binary-instances
                p.cabal-install
                p.concur-core
                p.concur-replica
                p.discrimination
                p.extra
                p.ghc-debug-common
                p.ghc-debug-stub
                p.ghc-debug-client
                p.ghc-debug-brick
                p.ghc-debug-convention
                p.hiedb
                p.hpack
                p.hspec
                p.lens
                p.pretty-simple
                p.OGDF
                p.optparse-applicative
                p.replica
                p.socket
                p.text
                p.time
                p.fourmolu
              ]);
          in pkgs.mkShell { packages = [ hsenv pkgs.nixfmt ]; };
        mkPackagesFor = compiler: {
          inherit (hpkgsFor compiler) ghc-specter-plugin ghc-specter-daemon ghc-build-analyzer;
        };
        supportedCompilers = [ "ghc924" "ghc942" ];
      in {
        inherit haskellOverlay;
        devShells = pkgs.lib.genAttrs supportedCompilers mkShellFor;
        packages = pkgs.lib.genAttrs supportedCompilers mkPackagesFor;
      });
}
