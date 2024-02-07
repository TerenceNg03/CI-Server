module Main (main) where

import Config (Config (logFile))
import Data.Yaml (decodeFileThrow)
import HandleLogger (withHandleLogger)
import Server (runServer)
import System.IO (IOMode (AppendMode), openFile, hClose)

main :: IO ()
main = do
    config <- decodeFileThrow "./config.yaml"
    handle <- openFile (logFile config) AppendMode
    withHandleLogger handle $ runServer config
    hClose handle
