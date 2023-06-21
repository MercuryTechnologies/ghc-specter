{
  description = "github-api test";
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
      fficxx-version = "0.7.0.0";
      HROOT-version = "0.10.0.2";
      hpkgs = pkgs.haskell.packages.ghc925.extend (hself: hsuper: {
        "fficxx" = hself.callHackage "fficxx" fficxx-version {};
        "fficxx-runtime" =
          hself.callHackage "fficxx-runtime" fficxx-version {};
        "stdcxx" = hself.callHackage "stdcxx" fficxx-version {};
        "template" = pkgs.haskell.lib.doJailbreak hsuper.template;
        "HROOT" =
          overrideCabal (hself.callHackage "HROOT" HROOT-version {}) {librarySystemDepends = [pkgs.root];};
        "HROOT-core" =
          overrideCabal (hself.callHackage "HROOT-core" HROOT-version {}) {librarySystemDepends = [pkgs.root];};
        "HROOT-graf" =
          overrideCabal (hself.callHackage "HROOT-graf" HROOT-version {}) {librarySystemDepends = [pkgs.root];};
        "HROOT-hist" =
          overrideCabal (hself.callHackage "HROOT-hist" HROOT-version {}) {librarySystemDepends = [pkgs.root];};
        "HROOT-io" =
          overrideCabal (hself.callHackage "HROOT-io" HROOT-version {}) {librarySystemDepends = [pkgs.root];};
        "HROOT-math" =
          overrideCabal (hself.callHackage "HROOT-math" HROOT-version {}) {librarySystemDepends = [pkgs.root];};
        "HROOT-net" =
          overrideCabal (hself.callHackage "HROOT-net" HROOT-version {RHTTP = null;}) {librarySystemDepends = [pkgs.root];};
        "HROOT-tree" =
          overrideCabal (hself.callHackage "HROOT-tree" HROOT-version {}) {librarySystemDepends = [pkgs.root];};

        "ormolu" =
          overrideCabal hsuper.ormolu {enableSeparateBinOutput = false;};
      });
      HROOTdeps = p: [
        p.fficxx
        p.fficxx-runtime
        p.stdcxx
        p.HROOT
        #p.template
      ];

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
          export PS1="\n[github-api:\w]$ \0"
        '';
      };
    });
}
