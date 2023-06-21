{
  description = "shell for logcat/util";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/master";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {inherit system;};
      fontconf = pkgs.makeFontsConf { fontDirectories = [pkgs.freefont_ttf]; };
      overrideCabal = pkgs.haskell.lib.overrideCabal;
      hpkgs = pkgs.haskell.packages.ghc925.extend (hself: hsuper: {
        "ormolu" =
          overrideCabal hsuper.ormolu {enableSeparateBinOutput = false;};
      });

      hsenv = hpkgs.ghcWithPackages (p:
        [
          p.aeson
          p.directory
          p.filepath
          p.gi-cairo
          p.gi-cairo-connector
          p.gi-cairo-render
          p.gi-gdk
          p.gi-gtk
          p.gi-gtk-hs
          p.gi-pango
          p.gi-pangocairo
          #p.haskell-gi-base
          p.lens
          p.lens-aeson
          p.pretty-simple
          p.tagsoup
          p.text
          p.time
          p.vector
        ]);
        #++ HROOTdeps p);
    in {
      devShells.default = pkgs.mkShell {
        name = "testshell";
        buildInputs = [
          hsenv
          pkgs.gh
          pkgs.jq
          hpkgs.ormolu
          pkgs.root
          pkgs.alejandra
        ];
        shellHook = ''
          export MACOSX_DEPLOYMENT_TARGET="10.16"
          export FONTCONFIG_FILE="${fontconf}"
          export PANGOCAIRO_BACKEND=fc
          export PS1="\n[logcat/util:\w]$ \0"
        '';
      };
    });
}
