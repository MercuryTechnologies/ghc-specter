{
  description = "Inspecting tool for the GHC pipeline through GHC plugin";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
    hackage-index = {
      type = "file";
      flake = false;
      url = "https://api.github.com/repos/commercialhaskell/all-cabal-hashes/tarball/13a91ab76cfac2098eff2780f3d3b6224352a7a2";
    };
  };
  outputs = inputs @ {
    self,
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        config.allowBroken = true;
      };

      haskellOverlay = final: hself: hsuper: {
        #"criterion" = final.haskell.lib.dontCheck hsuper.criterion;
        "discrimination" = hself.callHackage "discrimination" "0.5" {};
        #"http2" = final.haskell.lib.dontCheck hsuper.http2;
        #"newtype-generics" =
        #  final.haskell.lib.doJailbreak hsuper.newtype-generics;
        #"retry" = final.haskell.lib.dontCheck hsuper.retry;

        # fficxx-related
        "fficxx" = hself.callHackage "fficxx" "0.7.0.0" {};
        "fficxx-runtime" = hself.callHackage "fficxx-runtime" "0.7.0.0" {};
        "stdcxx" = hself.callHackage "stdcxx" "0.7.0.0" {};
        # libOGDF is bundled with libCOIN, so remove COIN dependency.
        "OGDF" = hself.callCabal2nix "OGDF" inputs.OGDF {
          COIN = null;
          OGDF = pkgs.ogdf;
        };
        "template" = final.haskell.lib.doJailbreak hsuper.template;
      };

      hpkgsFor = compiler:
        pkgs.haskell.packages.${compiler}.extend
        (hself: hsuper: haskellOverlay pkgs hself hsuper);

      mkShellFor = compiler: let
        hsenv = (hpkgsFor compiler).ghcWithPackages (p: []);
        pyenv =
          pkgs.python3.withPackages
          (p: [p.sphinx p.sphinx_rtd_theme p.myst-parser]);
      in
        pkgs.mkShell {
          packages = [
            hsenv
            pyenv
            pkgs.alejandra
            pkgs.cabal-install
            pkgs.ormolu
          ];
          shellHook = ''
            export PS1="\n[ghc-specter:\w]$ \0"
          '';
        };
      supportedCompilers = ["ghc962"];
    in {
      inherit haskellOverlay;
      devShells =
        pkgs.lib.genAttrs supportedCompilers mkShellFor;
    });
}
