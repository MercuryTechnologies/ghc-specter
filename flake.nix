{
  description = "Inspecting tool for the GHC pipeline through GHC plugin";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
    nixGL = {
      url = "github:guibou/nixGL/main";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
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
        nixGL.follows = "nixGL";
        fficxx.follows = "fficxx";
      };
    };
  };
  outputs = inputs @ {
    self,
    nixpkgs,
    flake-utils,
    nixGL,
    fficxx,
    hs-ogdf,
    hs-imgui,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          (hs-imgui.overlay.${system})
        ];
      };

      haskellOverlay = final: hself: hsuper: {
        "criterion" = final.haskell.lib.doJailbreak hsuper.criterion;
        "discrimination" = hself.callHackage "discrimination" "0.5" {};
        "vector-binary-instances" = final.haskell.lib.doJailbreak hsuper.vector-binary-instances;

        # ghc-specter-*
        "ghc-specter-plugin" =
          hself.callCabal2nix "ghc-specter-plugin" ./plugin {};
        "ghc-specter-daemon" =
          final.haskell.lib.overrideCabal
          (hself.callCabal2nix "ghc-specter-daemon" ./daemon {})
          (old: {
            libraryFrameworkDepends =
              final.lib.optional pkgs.stdenv.isDarwin final.darwin.apple_sdk.frameworks.Cocoa;
          });
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

      mkEnvShellFor = compiler: let
        hsenv = (hpkgsFor compiler).ghcWithPackages (
          p: [
            p.ghc-specter-daemon
            p.ghc-specter-plugin
          ]
        );
        prompt = "ghc-specter:env";
      in
        pkgs.mkShell {
          packages =
            [
              hsenv
              pkgs.cabal-install
              pkgs.alejandra
              pkgs.ormolu
              pkgs.zlib
            ]
            ++ (pkgs.lib.optional (pkgs.stdenv.isLinux && !pkgs.lib.inPureEvalMode) nixGL.packages.${system}.default);
          shellHook = ''
            export PS1="\n[${prompt}:\w]$ \0"
          '';
        };

      mkDevShellFor = compiler: let
        pyenv =
          pkgs.python3.withPackages
          (p: [p.sphinx p.sphinx_rtd_theme p.myst-parser]);
        prompt = "ghc-specter:dev";
      in
        (hpkgsFor compiler).shellFor {
          packages = p: [
            p.ghc-specter-plugin
            p.ghc-specter-daemon
          ];
          buildInputs =
            [
              pkgs.cabal-install
              pkgs.alejandra
              pkgs.ormolu
              pkgs.zlib
              pyenv
            ]
            ++ (
              pkgs.lib.optionals (pkgs.stdenv.isLinux && !pkgs.lib.inPureEvalMode)
              [
                nixGL.packages.${system}.default
                nixGL.packages.${system}.nixGLNvidia
                nixGL.packages.${system}.nixGLNvidiaBumblebee
                nixGL.packages.${system}.nixGLIntel
                nixGL.packages.${system}.nixVulkanNvidia
                nixGL.packages.${system}.nixVulkanIntel
              ]
            )
            ++ (pkgs.lib.optionals (pkgs.stdenv.isDarwin) [pkgs.darwin.apple_sdk.frameworks.Cocoa]);
          shellHook = ''
            export PS1="\n[${prompt}:\w]$ \0"
          '';
        };

      mkPackagesFor = compiler: {
        inherit
          (hpkgsFor compiler)
          ghc-specter-plugin
          ghc-specter-daemon
          ghc-build-analyzer
          ;
      };

      defaultCompiler = "ghc962";
      supportedCompilers = ["ghc962"];
    in rec {
      inherit haskellOverlay;

      devShells =
        pkgs.lib.genAttrs supportedCompilers mkDevShellFor
        // {
          default = devShells.${defaultCompiler};
          env = mkEnvShellFor defaultCompiler;
        };

      packages = pkgs.lib.genAttrs supportedCompilers mkPackagesFor;
    });
}
