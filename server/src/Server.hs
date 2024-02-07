{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server (runServer) where

import Config (Config (..))
import Control.Exception (bracketOnError)
import Fmt (format)
import Log (LogT, defaultLogLevel, logInfo_, runLogT)
import Log.Logger (Logger)
import Network.Socket (Family (AF_INET), SockAddr (SockAddrInet), SocketType (Stream), bind, close, listen, socket, socketPort, tupleToHostAddress, withSocketsDo)
import Network.Wai (Request (rawPathInfo, remoteHost), requestMethod)
import Web.Scotty.Trans (ScottyT, defaultOptions, function, matchAny, next, request, scottySocketT)

-- | Dispatch requests based on patterns
dispatch :: Config -> ScottyT (LogT IO) ()
dispatch Config{} = do
    matchAny (function $ const $ Just []) $ do
        r <- request
        logInfo_ $ format "{} {} {}" (show $ remoteHost r) (show $ requestMethod r) (show $ rawPathInfo r)
        next

-- | Run web server with given config and logger
runServer :: Config -> Logger -> IO ()
runServer config@Config{..} logger = withSocketsDo $ bracketOnError newSocket close $ \sock -> do
    bind sock $ SockAddrInet (fromIntegral portNumber) $ tupleToHostAddress (0, 0, 0, 0)
    listen sock 1024
    port <- socketPort sock
    runLog $
        do
            logInfo_ $ format "Running server on 0.0.0.0:{}" (show port)
            logInfo_ $ format "Database file path: {}" dbFile
            scottySocketT defaultOptions sock runLog (dispatch config)
  where
    newSocket = socket AF_INET Stream 0
    runLog = runLogT "main" logger defaultLogLevel
