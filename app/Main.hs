module Main where

import GHCEventLogChrome

import System.Environment
import GHC.RTS.Events
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS

main :: IO ()
main = do
    fileNames <- getArgs
    results <- mapM readEventLogFromFile fileNames
    case processEventLogs <$> sequence results of
        Left error -> putStrLn error
        Right tracing -> BS.putStrLn $ encode tracing

