{
  description = "heapoverflow-ghc-debug-test";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = {self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
          hspkgs = pkgs.haskell.packages.ghc962.override (old: {
            overrides = hself: hsuper: {
              eventlog2html = pkgs.haskell.lib.doJailbreak (hself.callHackage "eventlog2html" "0.10.0" {});
              ghc-debug-common = pkgs.haskell.lib.doJailbreak (hself.callHackage "ghc-debug-common" "0.5.0.0" {});
              ghc-debug-stub = pkgs.haskell.lib.doJailbreak (hself.callHackage "ghc-debug-stub" "0.5.0.0" {});
              ghc-debug-convention = hself.callHackage "ghc-debug-convention" "0.5.0.0" {};
              ghc-debug-client =
                hself.callCabal2nix
                  "ghc-debug-client"
                  ((pkgs.fetchFromGitHub {
                      owner = "wavewave";
                      repo = "ghc-debug";
                      rev = "8bb0af460f13cf3bd616d85c0fc5fd0cbe34130d";
                      hash = "sha256-ls31DhCnclaFQJlCe0JXe23tJCsxuztg9jrzY31sHwI=";
                    }) + "/client"
                  )
                  {};
              ghc-events = hself.callHackage "ghc-events" "0.19.0.1" {};
              language-dot =
                pkgs.haskell.lib.dontCheck (
                  hself.callCabal2nix
                    "language-dot"
                    (pkgs.fetchFromGitHub {
                        owner = "bgamari";
                        repo = "language-dot";
                        rev = "1d9cc81bf81036700b7f4fce6e432954a8c4584d";
                        hash = "sha256-kC5GNawCHYKA+tHCfaa4fOGGf26Isqz/FiQvcTi6VXU=";
                    })
                    {}
                );
              vector-binary-instances = pkgs.haskell.lib.doJailbreak hsuper.vector-binary-instances;
            };
          });
          hsenv = hspkgs.ghcWithPackages (p: [
            p.ghc-debug-stub
            p.ghc-debug-client
            p.cabal-install
          ]);
      in {
        devShells.default =
          pkgs.mkShell {
            buildInputs = [hsenv];
          };
        }
    );
}
