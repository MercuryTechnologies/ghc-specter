{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -w #-}

module Plugin.GHCSpecter.Tasks.Common
  ( runGhciOnGhc,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Driver.Env (runInteractiveHsc)
import GHC.Driver.Env.Types (HscEnv (hsc_interp))
import GHC.Driver.Main
  ( getHscEnv,
    hscParseStmtWithLocation,
    hscParsedStmt,
  )
import GHC.Driver.Session (getDynFlags)
import GHC.Runtime.Eval (execOptions, execStmt)
import GHC.Runtime.Interpreter.Types (Interp (interpInstance), InterpInstance (..))
import GHCSpecter.Channel.Outbound.Types (ConsoleReply (..))
import GHCSpecter.Util.GHC (printPpr, showPpr)

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
            let stmt_text = "3" -- "3 GHC.Prelude.Basic.+ 4"
            maybe_stmt <- hscParseStmtWithLocation "<interactive>" 1 stmt_text
            case maybe_stmt of
              Nothing -> reply "parse failure: 3 + 4"
              Just stmt -> do
                let opts = execOptions
                env' <- getHscEnv
                r <- liftIO $ hscParsedStmt env' stmt
                case r of
                  Nothing -> reply "no result"
                  Just (ids, _, fix_env) -> do
                    dflags <- getDynFlags
                    let result_str = show (length ids) <> " : " <> showPpr dflags ids
                    reply ("result = " <> T.pack result_str)
