module Main (main) where

import Config (Config (dbFile, logFile))
import Data.Yaml (decodeFileThrow)
import HandleLogger (withHandleLogger)
import Server (runServer)
import System.Directory (makeAbsolute)
import System.Environment (getArgs)
import System.IO (IOMode (AppendMode), hClose, openFile)

main :: IO ()
main = do
    args <- getArgs
    let configFile =
            case args of
                (_ : f : _) -> f
                _ -> "./config.yaml"
    config <- decodeFileThrow configFile
    handle <- openFile (logFile config) AppendMode
    dbAbs <- makeAbsolute $ dbFile config
    logAbs <- makeAbsolute $ logFile config
    withHandleLogger handle $
        runServer
            config
                { dbFile = dbAbs
                , logFile = logAbs
                }
    hClose handle
