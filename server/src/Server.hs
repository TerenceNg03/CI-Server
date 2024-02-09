{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Server (runServer, sha) where

import Config (Config (..))
import Crypto.Hash.SHA256 (hmac)
import Data.Aeson (eitherDecode)
import Data.ByteString (toStrict)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (find)
import Data.String (IsString (fromString))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy (fromStrict)
import Fmt (format)
import Log (LogT, defaultLogLevel, logInfo_, runLogT)
import Log.Logger (Logger)
import Network.HTTP.Types (status403)
import Network.Wai (Request (rawPathInfo, remoteHost), requestMethod)
import Web.Scotty.Trans (
    ScottyT,
    body,
    function,
    headers,
    html,
    matchAny,
    next,
    post,
    request,
    scottyT,
    status,
    text,
 )
import WebHook (Commit)

-- | Dispatch requests based on patterns
dispatch :: Config -> ScottyT (LogT IO) ()
dispatch Config{..} = do
    matchAny (function $ const $ Just []) $ do
        r <- request
        h <- headers
        b <- body
        logInfo_ $
            format
                "{} {} {}\n    Header: {}\n    Body: {}"
                (show $ remoteHost r)
                (show $ requestMethod r)
                (show $ rawPathInfo r)
                (show h)
                (show b)
        next
    post "/" $ do
        h <- headers
        payload <- body
        let verify = find (== ("X-Hub-Signature-256", fromStrict $ sha webSecret payload)) h
        case verify of
            Nothing -> do
                logInfo_ "Signature verification failed"
                status status403
                html "<h1>Invalid Signature</h1>"
            Just _ -> do
                let commit = eitherDecode @Commit payload
                logInfo_ $ pack $ show commit
                text "Accepted"

-- | Calculate HMAC from secret and payload
sha :: (IsString a) => Text -> ByteString -> a
sha secret payload = fromString . unpack $ "sha256=" <> decodeHex signature
  where
    decodeHex = decodeUtf8 . toStrict . toLazyByteString . byteStringHex
    signature = hmac (encodeUtf8 secret) $ toStrict payload

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
