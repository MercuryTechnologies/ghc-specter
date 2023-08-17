{-# LANGUAGE OverloadedStrings #-}

module GHCSpecter.UI.Help
  ( -- * list of allowed console commands
    consoleCommandList,
  )
where

import Data.Text (Text)
import GHCSpecter.Channel.Outbound.Types (BreakpointLoc (..))

-- | list of allowed console commands
consoleCommandList :: BreakpointLoc -> [Text]
consoleCommandList bp =
  case bp of
    StartDriver -> [":next", ":dump-heap", ":exit-ghc-debug"]
    ParsedResultAction -> [":next", ":dump-heap", ":exit-ghc-debug", ":goto-source"]
    RenamedResultAction -> [":next", ":dump-heap", ":exit-ghc-debug", ":goto-source", ":show-renamed"]
    PreRunMeta -> [":next", ":dump-heap", ":exit-ghc-debug", ":goto-source", ":show-expr"]
    SpliceRunAction -> [":next", ":dump-heap", ":exit-ghc-debug", ":goto-source", ":show-expr"]
    RnSplice -> [":next", ":dump-heap", ":exit-ghc-debug", ":goto-source", ":show-splice"]
    PostRunMeta -> [":next", ":dump-heap", ":exit-ghc-debug", ":goto-source", ":show-result"]
    TypecheckInit -> [":next", ":dump-heap", ":exit-ghc-debug", ":goto-source"]
    TypecheckSolve -> [":next", ":dump-heap", ":exit-ghc-debug", ":goto-source"]
    TypecheckStop -> [":next", ":dump-heap", ":exit-ghc-debug", ":goto-source"]
    TypecheckResultAction -> [":next", ":dump-heap", ":exit-ghc-debug", ":goto-source", ":unqualified"]
    Core2Core {} -> [":next", ":dump-heap", ":exit-ghc-debug", ":goto-source", ":list-core", ":print-core"]
    PreRunPhase {} -> [":next", ":dump-heap", ":exit-ghc-debug", ":goto-source"]
    PostRunPhase {} -> [":next", ":dump-heap", ":exit-ghc-debug", ":goto-source"]
