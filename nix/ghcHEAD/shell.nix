{ ghc_nix, system, pkgs, ogdfLib }: let
   fontconf = pkgs.makeFontsConf { fontDirectories = [pkgs.freefont_ttf]; };
in {
  # ghc.nix shell
  ghcNixShell = ghc_nix.outputs.devShells.${system}.default.overrideAttrs
    (attrs: {
      buildInputs = attrs.buildInputs
      ++ [
        pkgs.epoxy.dev
        pkgs.gd
        pkgs.gobject-introspection
        pkgs.gtk3
        pkgs.libdatrie
        pkgs.libdeflate
        pkgs.librsvg.dev
        pkgs.libthai
        pkgs.pcre
        pkgs.pcre2
        pkgs.xorg.libXdmcp.dev
        pkgs.libxkbcommon.dev
        pkgs.xorg.libXtst
        pkgs.pkgconfig
        ogdfLib

        pkgs.socat
      ]
      ++ pkgs.lib.optional pkgs.stdenv.isLinux [pkgs.libselinux.dev pkgs.libsepol.dev pkgs.util-linux.dev];


      shellHook = ''
        echo "ghc.nix shell hook"
        ${attrs.shellHook}
        export CXX=${pkgs.stdenv.cc}/bin/c++
        echo "now entering into ghcHEAD shell. Please set PATH to have your target GHC bin directory."
        export FONTCONFIG_FILE="${fontconf}"
        export PANGOCAIRO_BACKEND=fc
        export PS1="\n[ghc-specter:ghcHEAD:\w]$ \0"
      '';
    });
}
