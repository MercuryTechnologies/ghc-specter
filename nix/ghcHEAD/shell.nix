{ ghc_nix, system, pkgs, }: {
  # ghc.nix shell
  ghcNixShell = ghc_nix.outputs.devShells.${system}.default.overrideAttrs
    (attrs: {
      buildInputs = attrs.buildInputs ++ [
        #pkgs.bzip2
        #pkgs.expat
        #pkgs.geos
        #pkgs.gpgme
        #pkgs.icu
        #pkgs.libical
        #pkgs.libmcrypt
        #pkgs.libssh2
        #pkgs.openssl
        #pkgs.pcre
        pkgs.pkgconfig
      ];
      shellHook = ''
        echo "ghc.nix shell hook"
        ${attrs.shellHook}
        echo "now entering into ghcHEAD shell. Please set PATH to have your target GHC bin directory."
      '';
    });
}
