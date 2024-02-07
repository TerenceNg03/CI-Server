module Main (main) where
import Data.Yaml (decodeFileThrow)
import Server (runServer)
import Log.Backend.StandardOutput.Bulk (withBulkStdOutLogger)

main :: IO ()
main = do
    config <- decodeFileThrow "./config.yaml"
    withBulkStdOutLogger $ runServer config
