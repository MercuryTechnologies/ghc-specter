{-# LANGUAGE ApplicativeDo #-}
module Main where

import Options.Applicative
import Data.Aeson as A
import Data.Traversable (for)
import Data.ByteString.Lazy as B (readFile, split, toStrict)
import Data.Text.Encoding (decodeUtf8')
import Data.Char (ord)
import Lib

data Opts = Opts
    { file :: FilePath
    }
    deriving Show

optsParser :: ParserInfo Opts
optsParser =
    info (inner <**> helper)
        (fullDesc
        <> progDesc "Analyze GHC build output")
    where
        inner = do
            file <- strOption
                ( long "file"
                <> metavar "FILE"
                <> help "Filename to read GHC log from" )
            return Opts { file }

main :: IO ()
main = do
    opts <- execParser optsParser

    logLines <- B.split (fromIntegral . ord $ '\n') <$> B.readFile (file opts)

    for logLines $ \line -> do
        case decode line :: Maybe GhcMessage of
          Just message -> putStrLn $ show message
          Nothing -> putStrLn $ "Not a JSON message: " ++ (show $ decodeUtf8' $ toStrict line)

    return ()
