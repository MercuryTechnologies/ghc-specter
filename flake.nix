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
        # this is temporary. need to use nix expression directly from hs-imgui.
        config = {
          allowBroken = true;
          packageOverrides = self: {
            imgui = self.callPackage ./nix/imgui/default.nix {
              frameworks = self.darwin.apple_sdk.frameworks;
            };
            implot = self.callPackage ./nix/implot/default.nix {
              frameworks = self.darwin.apple_sdk.frameworks;
            };
          };
        };
      };

      haskellOverlay = final: hself: hsuper: {
        "criterion" = final.haskell.lib.doJailbreak hsuper.criterion;
        "discrimination" = hself.callHackage "discrimination" "0.5" {};
        "vector-binary-instances" = final.haskell.lib.doJailbreak hsuper.vector-binary-instances;

        # fficxx-related
        "fficxx" = hself.callHackage "fficxx" "0.7.0.0" {};
        "fficxx-runtime" = hself.callHackage "fficxx-runtime" "0.7.0.0" {};
        "stdcxx" = hself.callHackage "stdcxx" "0.7.0.0" {};

        "OGDF" = hself.callHackage "OGDF" "1.0.0.0" {
          COIN = null;
          OGDF = pkgs.ogdf;
        };
        "template" = final.haskell.lib.doJailbreak hsuper.template;

        # ghc-specter-*
        "ghc-specter-plugin" =
          hself.callCabal2nix "ghc-specter-plugin" ./plugin {};
        "ghc-specter-render" =
          hself.callCabal2nix "ghc-specter-render" ./render {};
        "ghc-specter-daemon" =
          hself.callCabal2nix "ghc-specter-daemon" ./daemon {};
        "ghc-build-analyzer" =
          hself.callCabal2nix "ghc-build-analyzer" ./ghc-build-analyzer {};
      };

      hpkgsFor = compiler:
        pkgs.haskell.packages.${compiler}.extend
        (hself: hsuper: haskellOverlay pkgs hself hsuper);

      mkShellFor = compiler: let
        hsenv = (hpkgsFor compiler).ghcWithPackages (p: [
          p.OGDF
          p.hspec-discover
        ]);
        pyenv =
          pkgs.python3.withPackages
          (p: [p.sphinx p.sphinx_rtd_theme p.myst-parser]);
      in
        pkgs.mkShell {
          packages = [
            # for build
            hsenv
            pkgs.ogdf
            pkgs.cabal-install

            # for doc
            pyenv

            # for formatting
            pkgs.alejandra
            pkgs.ormolu

            # for agda
            pkgs.zlib

            # for socket testing
            pkgs.socat

            # for GUI
            pkgs.pkgconfig
            pkgs.imgui
            pkgs.implot
            pkgs.glfw
          ];
          shellHook = ''
            export PS1="\n[ghc-specter:\w]$ \0"
          '';
        };

      mkPackagesFor = compiler: {
        inherit
          (hpkgsFor compiler)
          ghc-specter-plugin
          ghc-specter-render
          ghc-specter-daemon
          ghc-build-analyzer
          ;
      };

      defaultCompiler = "ghc962";
      supportedCompilers = ["ghc962"];
    in rec {
      inherit haskellOverlay;

      devShells =
        pkgs.lib.genAttrs supportedCompilers mkShellFor
        // {default = devShells.${defaultCompiler};};

      packages = pkgs.lib.genAttrs supportedCompilers mkPackagesFor;
    });
}
