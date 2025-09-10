# ghc-specter
Inspecting tool for the [GHC](https://github.com/ghc/ghc) pipeline through GHC plugin

`ghc-specter` is an inspecting tool and visualizer of internal states of
the GHC compilation pipeline via GHC plugins and hooks. Haskell developers
often need investigation in the middle of the compilation, for example,
to identify blocking compilation steps dominating the build time or to
investigate a problematic [Template Haskell](https://wiki.haskell.org/Template_Haskell) 
code, but have to rely only on built-in GHC logging or resort to a custom 
modification of the GHC source code. As a GHC driver plugin, invoked by simple 
command-line flags, `ghc-specter` collects the relevant information directly 
inside from the GHC compilation and sends this to a live visualizer daemon.
The user can also pause and intervene the GHC process in the middle of 
compilation to inspect the GHC internal state and inspect the compilation 
process in an interactive session.

## How to use

### Run daemon
```
$ nix develop
$ cabal run ghc-specter-daemon:exe:ghc-specter-daemon
```

### Run GHC in a project
Run GHC with `-fplugin Plugin.GHCSpecter` and `-plugin-package ghc-specter-plugin`.
If `ghc-specter-plugin` is already specified as the dependency of the project,
the latter is not necessary.

Example:
```
$ cabal build --ghc-options "-fplugin Plugin.GHCSpecter -plugin-package ghc-specter-plugin"
```
