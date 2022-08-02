{-# LANGUAGE GADTs #-}

module Main (main) where

import Concur.Core (liftSTM)
import Concur.Replica (runDefault)
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    modifyTVar',
    newTVar,
    readTVar,
    retry,
  )
import Control.Monad.Extra (loopM)
import qualified Data.Map.Strict as M
import qualified Options.Applicative as OA
import Toolbox.Channel
  ( ChanMessage (CMCheckImports, CMTrivial),
    ChanMessageBox (..),
    Channel (..),
  )
import Toolbox.Comm
  ( receiveObject,
    runServer,
  )
import Toolbox.Render (render)
import Toolbox.Server.Types (Inbox)
import Prelude hiding (div)

newtype Options = Options {optSocketFile :: FilePath}

optsParser :: OA.ParserInfo Options
optsParser =
  OA.info
    (Options <$> OA.strOption (OA.long "socket-file" <> OA.short 's' <> OA.help "socket file"))
    OA.fullDesc

main :: IO ()
main = do
  opts <- OA.execParser optsParser
  let socketFile = optSocketFile opts
  var <- atomically $ newTVar (0, mempty)
  _ <- forkIO $ listener socketFile var
  webServer var

listener :: FilePath -> TVar (Int, Inbox) -> IO ()
listener socketFile var =
  runServer
    socketFile
    (\sock -> receiveObject sock >>= atomically . updateInbox var)

updateInbox :: TVar (Int, Inbox) -> ChanMessageBox -> STM ()
updateInbox var chanMsg =
  modifyTVar' var $ \(i, m) ->
    let (chan, modu, msg) =
          case chanMsg of
            CMBox (CMCheckImports m' t') -> (CheckImports, m', t')
            CMBox (CMTrivial m') -> (Trivial, m', "no-message")
     in (i + 1, M.insert (chan, modu) msg m)

webServer :: TVar (Int, Inbox) -> IO ()
webServer var = do
  initVal <- atomically (readTVar var)
  runDefault 8080 "test" $
    \_ -> loopM step ((CheckImports, Nothing), initVal)
  where
    step ((chan, mexpandedModu), (i, m)) = do
      let await = liftSTM $ do
            (i', m') <- readTVar var
            if i == i'
              then retry
              else pure (i', m')
      Left
        <$> ( render ((chan, mexpandedModu), (i, m))
                <|> (((chan, mexpandedModu),) <$> await)
            )
