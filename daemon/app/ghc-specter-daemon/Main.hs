module Main (main) where

import Control.Concurrent (forkOS)
import Control.Concurrent.STM
  ( atomically,
    newTChanIO,
    newTQueueIO,
    newTVar,
  )
import Data.Aeson (eitherDecode')
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import GHCSpecter.Config
  ( Config (..),
    defaultGhcSpecterConfigFile,
    loadConfig,
  )
import GHCSpecter.Driver.Comm qualified as Comm
import GHCSpecter.Driver.Session.Types (ServerSession (..))
import GHCSpecter.Driver.Web (webServer)
import GHCSpecter.Driver.Worker qualified as Worker
import GHCSpecter.Server.Types (initServerState)
import Options.Applicative qualified as OA

data CLIMode
  = Online (Maybe FilePath)
  | View (Maybe FilePath)

onlineMode :: OA.Mod OA.CommandFields CLIMode
onlineMode =
  OA.command "online" $
    OA.info
      ( Online
          <$> OA.optional
            (OA.strOption (OA.long "config" <> OA.short 'c' <> OA.help "config file"))
      )
      (OA.progDesc "GHC inspection on the fly")

viewMode :: OA.Mod OA.CommandFields CLIMode
viewMode =
  OA.command "view" $
    OA.info
      ( View
          <$> OA.optional
            (OA.strOption (OA.long "config" <> OA.short 'c' <> OA.help "config file"))
      )
      (OA.progDesc "viewing saved session")

optsParser :: OA.ParserInfo CLIMode
optsParser =
  OA.info
    (OA.subparser (onlineMode <> viewMode) OA.<**> OA.helper)
    OA.fullDesc

withConfig :: Maybe FilePath -> (Config -> IO ()) -> IO ()
withConfig mconfigFile action = do
  let config = fromMaybe defaultGhcSpecterConfigFile mconfigFile
  ecfg <- loadConfig config
  case ecfg of
    Left err -> putStrLn err
    Right cfg -> do
      print cfg
      action cfg

main :: IO ()
main = do
  mode <- OA.execParser optsParser
  case mode of
    Online mconfigFile ->
      withConfig mconfigFile $ \cfg -> do
        let socketFile = configSocket cfg
            nodeSizeLimit = configModuleClusterSize cfg
        ssRef <- atomically $ newTVar (initServerState nodeSizeLimit)
        workQ <- newTQueueIO
        chanSignal <- newTChanIO
        let servSess = ServerSession ssRef chanSignal
        _ <- forkOS $ Comm.listener socketFile servSess workQ
        _ <- forkOS $ Worker.runWorkQueue workQ
        webServer cfg servSess
    View mconfigFile ->
      withConfig mconfigFile $ \cfg -> do
        let sessionFile = configSessionFile cfg
        lbs <- BL.readFile sessionFile
        case eitherDecode' lbs of
          Left err -> print err
          Right ss -> do
            chanSignal <- newTChanIO
            ssRef <- atomically $ newTVar ss
            let servSess = ServerSession ssRef chanSignal
            webServer cfg servSess
