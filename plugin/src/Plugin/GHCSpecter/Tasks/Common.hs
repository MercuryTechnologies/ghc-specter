{-# LANGUAGE OverloadedStrings #-}

module Plugin.GHCSpecter.Tasks.Common
  ( runGhciOnGhc,
  )
where

import Data.Text (Text)
import GHC.Driver.Env (runInteractiveHsc)
import GHC.Driver.Env.Types (HscEnv (hsc_interp))
import GHC.Driver.Main (hscParseStmtWithLocation)
import GHC.Runtime.Interpreter.Types (Interp (interpInstance), InterpInstance (..))
import GHCSpecter.Channel.Outbound.Types (ConsoleReply (..))

runGhciOnGhc :: HscEnv -> IO ConsoleReply
runGhciOnGhc env = do
  let reply :: (Monad m) => Text -> m ConsoleReply
      reply = pure . ConsoleReplyText (Just "ghci")
  let minterp = hsc_interp env
  case minterp of
    Nothing -> reply "no-interpreter"
    Just interp ->
      case interpInstance interp of
        ExternalInterp {} -> reply "external interpreter is not supported."
        InternalInterp {} ->
          runInteractiveHsc env $ do
            maybe_stmt <- hscParseStmtWithLocation "<interactive>" 1 "3 + 4"
            case maybe_stmt of
              Nothing -> reply "parse failure: 3 + 4"
              Just stmt -> reply "success"
