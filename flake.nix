{
  description = "Inspecting tool for the GHC pipeline through GHC plugin";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
    fficxx = {
      url = "github:wavewave/fficxx/master";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
    hs-ogdf = {
      url = "github:wavewave/hs-ogdf/master";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        fficxx.follows = "fficxx";
      };
    };
    hs-imgui = {
      url = "github:wavewave/hs-imgui/main";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        fficxx.follows = "fficxx";
      };
    };
  };
  outputs = inputs @ {
    self,
    nixpkgs,
    flake-utils,
    fficxx,
    hs-ogdf,
    hs-imgui,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [hs-imgui.overlay.${system}];
      };

      haskellOverlay = final: hself: hsuper: {
        "criterion" = final.haskell.lib.doJailbreak hsuper.criterion;
        "discrimination" = hself.callHackage "discrimination" "0.5" {};
        "vector-binary-instances" = final.haskell.lib.doJailbreak hsuper.vector-binary-instances;

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
        (hself: hsuper:
          fficxx.haskellOverlay.${system} pkgs hself hsuper
          // hs-ogdf.haskellOverlay.${system} pkgs hself hsuper
          // hs-imgui.haskellOverlay.${system} pkgs hself hsuper
          // haskellOverlay pkgs hself hsuper);

      mkShellFor = isEnv: compiler: let
        hsenv = (hpkgsFor compiler).ghcWithPackages (
          p:
            [
              p.OGDF
              p.imgui
              p.implot
              p.hspec-discover
            ]
            ++ (pkgs.lib.optionals isEnv [p.ghc-specter-daemon])
        );
        pyenv =
          pkgs.python3.withPackages
          (p: [p.sphinx p.sphinx_rtd_theme p.myst-parser]);
        prompt =
          if isEnv
          then "ghc-specter:env"
          else "ghc-specter:dev";
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
            export PS1="\n[${prompt}:\w]$ \0"
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
        pkgs.lib.genAttrs supportedCompilers (mkShellFor false)
        // {
          default = devShells.${defaultCompiler};
          env = mkShellFor true defaultCompiler;
        };

      packages = pkgs.lib.genAttrs supportedCompilers mkPackagesFor;
    });
}
