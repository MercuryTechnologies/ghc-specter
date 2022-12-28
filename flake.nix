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
    file-embed = {
      url = "github:snoyberg/file-embed/file-embed-0.0.15.0";
      flake = false;
    };
    fourmolu = {
      url = "github:fourmolu/fourmolu/main";
      flake = false;
    };
    microlens = {
      url = "github:stevenfontanella/microlens/master";
      flake = false;
    };
    vty = {
      url = "github:jtdaugherty/vty/5.37";
      flake = false;
    };

  };
  outputs = { self, nixpkgs, flake-utils, concur, concur-replica, replica
    , fficxx, hs-ogdf, file-embed, fourmolu, microlens, vty }:
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

          # TODO: check whether this will be fixed in GHC 9.4.3.
          "conduit-extra" = final.haskell.lib.dontCheck hsuper.conduit-extra;
          "file-embed" = hself.callCabal2nix "file-embed" file-embed { };
          # fixity-th is disabled to avoid segfault during compilation.
          "fourmolu_0_9_0_0" = final.haskell.lib.dontCheck
            (final.haskell.lib.overrideCabal
              (hself.callCabal2nix "fourmolu" fourmolu { }) (drv: {
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
            hself.callCabal2nix "microlens" "${microlens}/microlens" { };
          "microlens-ghc" =
            hself.callCabal2nix "microlens-ghc" "${microlens}/microlens-ghc"
            { };
          "microlens-platform" = hself.callCabal2nix "microlens-platform"
            "${microlens}/microlens-platform" { };
          "string-qq" = final.haskell.lib.doJailbreak hsuper.string-qq;
          "vty" =
            final.haskell.lib.dontCheck (hself.callCabal2nix "vty" vty { });

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
        };

        hpkgsFor = compiler:
          pkgs.haskell.packages.${compiler}.extend (hself: hsuper:
            (fficxx.haskellOverlay.${system} pkgs hself hsuper)
            // (hs-ogdf.haskellOverlay.${system} pkgs hself hsuper)
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
                #p.ghc-debug-client
                # this seems outdated.
                # p.ghc-debugger
                #p.dyepack-test
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
              ] ++ (if compiler == "ghc942" then
                [ p.fourmolu_0_9_0_0 ]
              else
                [ p.fourmolu ]));
          in pkgs.mkShell { packages = [ hsenv pkgs.nixfmt ]; };

        supportedCompilers = [ "ghc924" "ghc942" ];
      in {
        inherit haskellOverlay;
        devShells = pkgs.lib.genAttrs supportedCompilers mkShellFor;
      });
}
