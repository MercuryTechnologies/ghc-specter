# Getting Started

## Configuration
One can configure ghc-specter by the file `ghc-specter.yaml`.
The file should be present in the directory where ghc-specter-daemon
runs and where a target `ghc` runs (usually the root directory of a
cabal project).
A sample config YAML file can be found in `ghc-specter.yaml.sample`.

## Run daemon
```
$ nix develop
$ cabal run ghc-specter-daemon:ghc-specter-daemon -- online
```

## Run GHC in a project
Run GHC with `-fplugin Plugin.GHCSpecter` and `-plugin-package ghc-specter-plugin`.
If `ghc-specter-plugin` is already specified as the dependency of the project,
the latter is not necessary.

Example:
```
$ cabal build --ghc-options "-fplugin Plugin.GHCSpecter -plugin-package ghc-specter-plugin"
```
