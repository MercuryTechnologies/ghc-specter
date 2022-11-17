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
  };
  outputs = { self, nixpkgs, flake-utils, concur, concur-replica, replica
    , fficxx, hs-ogdf, file-embed, fourmolu }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

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
          "fourmolu" = final.haskell.lib.dontCheck
            (final.haskell.lib.overrideCabal
              (hself.callCabal2nix "fourmolu" fourmolu { }) (drv: {
                configureFlags = [ "-f-fixity-th" ];
                libraryHaskellDepends = drv.libraryHaskellDepends
                  ++ [ hself.file-embed ];
              }));
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
              p.pretty-simple
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
