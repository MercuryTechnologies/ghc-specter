module GHCEvents (extract) where

import Data.ByteString.Lazy qualified as BL
import GHC.RTS.Events (EventLog (..))
import GHC.RTS.Events.Incremental (readEventLog)

extract :: FilePath -> IO (Either String (EventLog, Maybe String))
extract fp = do
  lbs <- BL.readFile fp
  pure $ readEventLog lbs
