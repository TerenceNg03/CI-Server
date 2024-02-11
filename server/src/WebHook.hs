{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module WebHook (Commit (..), Repo (..), runWebHook, invokeMavenCommand) where

import Config (Config (Config, dbFile, domain, githubToken))
import Control.Concurrent (forkIO)
import Control.Exception
import Control.Monad (void)
import Control.Monad.Error.Class (MonadError (catchError))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Data.Aeson (
    FromJSON,
    Options (constructorTagModifier, fieldLabelModifier),
    ToJSON,
    camelTo2,
    defaultOptions,
    genericParseJSON,
    genericToEncoding,
    parseJSON,
    toEncoding,
 )
import Data.Foldable (foldl')
import Data.Text (Text, pack, replace, splitOn)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID (UUID, toText)
import Database (Build (..), insertBuild)
import Database.Persist.Sqlite (runSqlite)
import Fmt (format)
import GHC.Generics (Generic)
import Log (LogT, Logger, defaultLogLevel, logAttention_, logInfo_, runLogT)
import Network.HTTP.Req (POST (POST), ReqBodyJson (ReqBodyJson), bsResponse, defaultHttpConfig, header, https, req, responseBody, runReq, (/:))
import System.Exit (ExitCode (..))
import System.Process (cwd, readCreateProcessWithExitCode, shell)

-- | Commit Info
data Commit = Commit
    { after :: Text
    -- ^ SHA of the commit
    , repository :: Repo
    -- ^ Repository of the commit
    }
    deriving (Generic, Show, Eq)

-- | Repository Info
data Repo = Repo
    { cloneUrl :: Text
    -- ^ Url used by `git clone`
    , statusesUrl :: Text
    -- ^ Url to post commit status
    }
    deriving (Generic, Show, Eq)

jsonOptions :: Options
jsonOptions =
    defaultOptions
        { fieldLabelModifier = camelTo2 '_'
        , constructorTagModifier = camelTo2 '_'
        }

instance ToJSON Commit where
    toEncoding = genericToEncoding jsonOptions

instance FromJSON Commit

instance ToJSON Repo where
    toEncoding = genericToEncoding jsonOptions

instance FromJSON Repo where
    parseJSON = genericParseJSON jsonOptions

data Response = Response
    { state :: State
    , targetUrl :: Text
    , description :: Text
    , context :: Text
    }
    deriving (Show, Generic)

instance ToJSON Response where
    toEncoding = genericToEncoding jsonOptions

-- | Commit status state
data State = Error | Failure | Pending | Success
    deriving (Generic)

instance ToJSON State where
    toEncoding = genericToEncoding jsonOptions

instance Show State where
    show s = case s of
        Error -> "error"
        Failure -> "failure"
        Pending -> "pending"
        Success -> "success"

invokeMavenCommand :: [String] -> IO (Either String (Int, String))
invokeMavenCommand args = do
    let mvnCommand = shell $ unwords ("mvn" : args)
    result <- readCreateProcessWithExitCode mvnCommand{cwd = Just "../"} ""
    return $ case result of
        (ExitSuccess, stdout, _) -> Right (0, stdout)
        (ExitFailure code, _, stderr) -> Left (stderr ++ "\nExit code: " ++ show code)

postStatus :: UUID -> Text -> Text -> Commit -> State -> Text -> LogT IO ()
postStatus uuid token domain commit s desc = flip catchError (logInfo_ . pack . show) $ do
    logInfo_ $
        format
            "Sending commit status:\n    state: {}\n    url: {}\n    body: {}"
            (show s)
            (show url)
            (show response)
    rsp <-
        lift
            $ handle
                (return . Left . pack . show @SomeException)
            $ Right <$> runReq defaultHttpConfig (req POST url (ReqBodyJson response) bsResponse headers)
    case rsp of
        Left err -> logAttention_ err
        Right resp -> logInfo_ $ decodeUtf8 $ responseBody resp
  where
    uri = replace "https://" "" $ replace "{sha}" (after commit) $ statusesUrl $ repository commit
    uris = splitOn "/" uri
    url = foldl' (/:) (https (head uris)) $ tail uris
    response =
        Response
            s
            (domain <> "/build/" <> toText uuid)
            desc
            "se-group9/ci-server"
    headers =
        header "Accept" "application/vnd.github+json"
            <> header "Authorization" (format "token {}" token)
            <> header "X-GitHub-Api-Version" "2022-11-28"
            <> header "User-Agent" "se-group9-ci"

-- | Run webhook jobs and post status to github
runWebHook :: UUID -> Logger -> Config -> Commit -> IO ()
runWebHook uuid logger config@Config{..} commit = do
    void
        $ forkIO
        $ runLogT
            (format "worker({})" $ toText uuid)
            logger
            defaultLogLevel
        $ do
            postStatus' Pending "Working on checks..."
            compileResult <- runMaven uuid commit config ["-B", "-DskipTests", "compile"]
            testResult <- runMaven uuid commit config ["-B", "test"]
            currentTime <- liftIO getCurrentTime
            liftIO $
                runSql $
                    insertBuild $
                        Build
                            (toText uuid)
                            (after commit)
                            (pack $ formatTime defaultTimeLocale "%F %T %z" currentTime)
                            (compileResult <> testResult)
  where
    postStatus' = postStatus uuid githubToken domain commit
    runSql = runSqlite $ pack dbFile

runMaven :: UUID -> Commit -> Config -> [String] -> LogT IO (Text)
runMaven uuid commit Config{..} command = do
    result <- liftIO $ invokeMavenCommand command
    case result of
        Left errMsg -> do
            logAttention_ $ format "Failed to run {}: {}" command errMsg
            postStatus' Error "Failed to compile!"
            return (pack errMsg)
        Right (exitCode, output) -> do
            logInfo_ $ format "{} succeeded with exit code: {}" command exitCode
            postStatus' Success "Compilation succeeded!"
            return (pack output)
  where
    postStatus' = postStatus uuid githubToken domain commit
