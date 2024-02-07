module Main (main) where

import Config (Config (logFile))
import Data.Yaml (decodeFileThrow)
import FileLogger (withFileLogger)
import Server (runServer)

main :: IO ()
main = do
    config <- decodeFileThrow "./config.yaml"
    withFileLogger (logFile config) $ runServer config
