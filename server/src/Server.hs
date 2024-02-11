{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Server (runServer, sha) where

import Config (Config (..))
import Control.Monad.Except (catchError)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), lift, withReaderT)
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
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Database (getBuildByUUID, getBuilds, migrateAll)
import Database.Persist.Sqlite (Entity (entityVal), runMigration, runSqlite)
import Fmt (format)
import Log (LogT, MonadLog (localDomain), defaultLogLevel, logInfo_, runLogT)
import Log.Logger (Logger)
import Network.HTTP.Types (status403, status404)
import Network.Wai (Request (rawPathInfo, remoteHost), requestMethod)
import Web.Scotty.Trans (
    ScottyT,
    body,
    function,
    get,
    headers,
    html,
    json,
    liftIO,
    matchAny,
    next,
    pathParam,
    post,
    request,
    scottyT,
    status,
    text,
 )
import WebHook (Commit, runWebHook)

-- | Dispatch requests based on patterns
dispatch :: ScottyT (ReaderT Config (LogT IO)) ()
dispatch = do
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
    get "/build/:id" $ do
        uuid <- pathParam "id"
        build <- runSql $ getBuildByUUID uuid
        case build of
            Just b -> json $ entityVal b
            Nothing -> do
                status status404
                html "<h1>Invalid build id</h1>"
    get "/builds" $ do
        builds <- runSql getBuilds
        json $ entityVal <$> builds
    post "/" $ flip catchError (logInfo_ . pack . show) $ do
        h <- headers
        payload <- body
        uuid <- liftIO nextRandom
        Config{..} <- lift ask
        localDomain (toText uuid) $ do
            let verify = find (== ("X-Hub-Signature-256", fromStrict $ sha webSecret payload)) h
                commitRaw = eitherDecode @Commit payload
            case (verify, commitRaw) of
                (Nothing, _) -> do
                    logInfo_ "Signature verification failed"
                    status status403
                    html "<h1>Invalid Signature</h1>"
                (Just _, Left msg) -> do
                    logInfo_ $ format "Payload decoding failed: {}" msg
                    status status403
                    html "<h1>Invalid Payload</h1>"
                (Just _, Right commit) -> do
                    logInfo_ $ pack $ show commit
                    lift $ withReaderT (,commit,uuid) runWebHook
                    text "Accepted"
  where
    runSql a = do
        Config{..} <- lift ask
        runSqlite (pack dbFile) a

-- | Calculate HMAC from secret and payload
sha :: (IsString a) => Text -> ByteString -> a
sha secret payload = fromString . unpack $ "sha256=" <> decodeHex signature
  where
    decodeHex = decodeUtf8 . toStrict . toLazyByteString . byteStringHex
    signature = hmac (encodeUtf8 secret) $ toStrict payload

-- | Run web server with given config and logger
runServer :: Config -> Logger -> IO ()
runServer config@Config{..} logger = do
    runLog "main" $
        do
            runSqlite (pack dbFile) (runMigration migrateAll)
            logInfo_ $ format "Running server on 0.0.0.0:{}" (show portNumber)
            logInfo_ $ format "Database file path: {}" dbFile
            logInfo_ $ format "Log file path: {}" logFile
            scottyT portNumber (runLog "worker" . flip runReaderT config) dispatch
  where
    runLog name = runLogT name logger defaultLogLevel
