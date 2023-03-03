{ ghc_nix, system, pkgs, ogdfLib }: {
  # ghc.nix shell
  ghcNixShell = ghc_nix.outputs.devShells.${system}.default.overrideAttrs
    (attrs: {
      buildInputs = attrs.buildInputs ++ [
        ogdfLib
        pkgs.pkgconfig
      ];
      OGDF=ogdfLib;
      shellHook = ''
        echo "ghc.nix shell hook"
        ${attrs.shellHook}
        echo "now entering into ghcHEAD shell. Please set PATH to have your target GHC bin directory."
        echo ${ogdfLib}
      '';
    });
}
