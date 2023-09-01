{-# LANGUAGE OverloadedStrings #-}

module Plugin.GHCSpecter.Tasks.Common
  ( runGhciOnGhc,
  )
where

import GHC.Driver.Env (runInteractiveHsc)
import GHC.Driver.Env.Types (HscEnv (hsc_interp))
import GHC.Runtime.Interpreter.Types (Interp (interpInstance), InterpInstance (..))
import GHCSpecter.Channel.Outbound.Types (ConsoleReply (..))

runGhciOnGhc :: HscEnv -> IO ConsoleReply
runGhciOnGhc env = do
  let minterp = hsc_interp env
  case minterp of
    Nothing ->
      pure (ConsoleReplyText (Just "ghci") "no-interpreter")
    Just interp ->
      case interpInstance interp of
        ExternalInterp {} ->
          pure (ConsoleReplyText (Just "ghci") "external interpreter is not supported.")
        InternalInterp {} ->
          runInteractiveHsc env $ do
            pure (ConsoleReplyText (Just "ghci") "I am inside interpreter")
