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
    let mvnCommand = shell $ unwords ("mvn":args)
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
            handleCompileResult uuid commit config
            handleTestingResult uuid commit config
  where
    postStatus' = postStatus uuid githubToken domain commit

handleCompileResult :: UUID -> Commit -> Config -> LogT IO ()
handleCompileResult uuid commit Config{..} = do
    postStatus' Pending "Compilation in progress..."
    compileResult <- liftIO $ invokeMavenCommand ["-B", "-q", "-DskipTests", "compile"]
    case compileResult of
        Left errMsg -> do
            logAttention_ $ format "Failed to compile: {}" errMsg
            postStatus' Error "Compilation failed!"
        Right (exitCode, compileOutput) -> do
            currentTime <- liftIO getCurrentTime
            liftIO $
                runSql $
                    insertBuild $
                        Build
                            (toText uuid)
                            (after commit)
                            (pack $ formatTime defaultTimeLocale "%F %T %z" currentTime)
                            (pack compileOutput)
            logInfo_ $ format "Compilation succeeded with exit code: {}" exitCode
            postStatus' Success "Compilation succeeded!"
  where
    postStatus' = postStatus uuid githubToken domain commit
    runSql = runSqlite $ pack dbFile

handleTestingResult :: UUID -> Commit -> Config -> LogT IO ()
handleTestingResult uuid commit Config{..} = do
    postStatus' Pending "Testing in progress..."
    testResult <- liftIO $ invokeMavenCommand ["-B", "test"]
    case testResult of
        Left errMsg -> do
            logAttention_ $ format "Failed to test: {}" errMsg
            postStatus' Error "Testing failed!"
        Right (exitCode, _) -> do
            logInfo_ $ format "Testing succeeded with exit code: {}" exitCode
            postStatus' Success "Testing succeeded!"
  where
    postStatus' = postStatus uuid githubToken domain commit
