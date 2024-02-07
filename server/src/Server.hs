{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server (runServer) where

import Config (Config (..))
import Data.Maybe (fromMaybe)
import Fmt (format)
import Log (LogT, defaultLogLevel, logInfo_, runLogT)
import Log.Logger (Logger)
import Network.Wai (Request (rawPathInfo, remoteHost, requestHeaderHost), requestMethod, strictRequestBody)
import Web.Scotty.Trans (ScottyT, function, liftIO, matchAny, next, request, scottyT)

-- | Dispatch requests based on patterns
dispatch :: Config -> ScottyT (LogT IO) ()
dispatch Config{} = do
    matchAny (function $ const $ Just []) $ do
        r <- request
        body <- liftIO $ strictRequestBody r
        logInfo_ $
            format
                "{} {} {}\n    Header: {}\n    Body: {}"
                (show $ remoteHost r)
                (show $ requestMethod r)
                (show $ rawPathInfo r)
                (show $ fromMaybe "" $ requestHeaderHost r)
                (show body)
        next

-- | Run web server with given config and logger
runServer :: Config -> Logger -> IO ()
runServer config@Config{..} logger = do
    runLog $
        do
            logInfo_ $ format "Running server on 0.0.0.0:{}" (show portNumber)
            logInfo_ $ format "Database file path: {}" dbFile
            logInfo_ $ format "Log file path: {}" logFile
            scottyT portNumber runLog (dispatch config)
  where
    runLog = runLogT "main" logger defaultLogLevel
