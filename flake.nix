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
    ghc-debug = {
      url =
        "git+https://gitlab.haskell.org/wavewave/ghc-debug.git?ref=wavewave/ghc94";
      flake = false;
    };
  };
  outputs = { self, nixpkgs, flake-utils, concur, concur-replica, replica
    , fficxx, hs-ogdf, file-embed, fourmolu, ghc-debug }:
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
          # ghc-debug
          "ghc-debug-common" =
            hself.callCabal2nix "ghc-debug-common" "${ghc-debug}/common" { };
          "ghc-debug-stub" = final.haskell.lib.doJailbreak
            (hself.callCabal2nix "ghc-debug-stub" "${ghc-debug}/stub" { });
          "ghc-debug-client" =
            hself.callCabal2nix "ghc-debug-client" "${ghc-debug}/client" { };
          "ghc-debug-test" =
            hself.callCabal2nix "ghc-debug-test" "${ghc-debug}/test" { };
          "ghc-debug-dyepack-test" =
            hself.callCabal2nix "ghc-debug-dyepack-test"
            "${ghc-debug}/dyepack-test" { };
          #"ghc-debug-brick" = hself.callCabal2nix "ghc-debug-brick" "${ghc-debug}/brick" { };
          "ghc-debug-convention" =
            hself.callCabal2nix "ghc-debug-convention" "${ghc-debug}/convention"
            { };
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
                #p.ghc-debug-test
                #p.ghc-debug-dyepack-test
                #p.ghc-debug-brick
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
