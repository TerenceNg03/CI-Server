{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module WebHook (Commit (..), Repo (..), runWebHook, invokeMavenCommand) where

import Config (Config (Config, dbFile, domain, githubToken))
import Control.Concurrent (ThreadId, forkIO)
import Control.Exception
import Control.Monad (void)
import Control.Monad.Error.Class (MonadError (catchError))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
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
import Log (LogT, LoggerEnv (LoggerEnv, leComponent, leDomain, leLogger, leMaxLogLevel), MonadLog (getLoggerEnv, localDomain), logAttention_, logInfo_, runLogT)
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

type HookM = ReaderT (Config, Commit, UUID) (LogT IO)

-- | Run HookM in another thread
forkHookIO :: HookM a -> HookM ThreadId
forkHookIO a = do
    context <- ask
    LoggerEnv{..} <- getLoggerEnv
    liftIO $
        forkIO $
            runLogT leComponent leLogger leMaxLogLevel $
                flip runReaderT context $
                    foldl
                        (flip localDomain)
                        (void a)
                        leDomain

invokeMavenCommand :: [String] -> IO (String, Maybe Int)
invokeMavenCommand args = do
    let mvnCommand = shell $ unwords ("mvn" : args)
    result <- readCreateProcessWithExitCode mvnCommand{cwd = Just "../"} ""
    return $ case result of
        (ExitSuccess, stdout, _) -> (stdout, Nothing)
        (ExitFailure code, stdout, _) -> (stdout, Just code)

postStatus :: State -> Text -> HookM ()
postStatus s desc = localDomain "postStatus" $ flip catchError (logInfo_ . pack . show) $ do
    (Config{..}, Commit{..}, uuid) <- ask

    let uri = replace "https://" "" $ replace "{sha}" after $ statusesUrl repository
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
                <> header "Authorization" (format "token {}" githubToken)
                <> header "X-GitHub-Api-Version" "2022-11-28"
                <> header "User-Agent" "se-group9-ci"

    logInfo_ $
        format
            "Sending commit status:\n    state: {}\n    url: {}\n    body: {}"
            (show s)
            (show url)
            (show response)
    rsp <-
        lift
            $ lift
            $ handle
                (return . Left . pack . show @SomeException)
            $ Right <$> runReq defaultHttpConfig (req POST url (ReqBodyJson response) bsResponse headers)
    case rsp of
        Left err -> logAttention_ err
        Right resp -> logInfo_ $ decodeUtf8 $ responseBody resp

-- | Run webhook jobs and post status to github
runWebHook :: HookM ()
runWebHook = void $ forkHookIO $ do
    (Config{..}, _, _) <- ask
    postStatus Pending "Working on checks..."
    (status, build) <- runMaven
    runSqlite (pack dbFile) $ insertBuild build
    case status of
        Left msg -> postStatus Failure msg
        Right msg -> postStatus Success msg

runMaven :: HookM (Either Text Text, Build)
runMaven = do
    (_, Commit{..}, uuid) <- ask
    let args = format "-Dexec.args=\"'{}' '{}'\"" (cloneUrl repository) after
    result <- liftIO $ invokeMavenCommand ["-q", "exec:java", args]
    time <- liftIO getCurrentTime
    let st =
            case snd result of
                Nothing -> Right "Check succeeded"
                Just c -> Left $ format "Check failed with exit code {}" c

        build =
            Build
                (toText uuid)
                after
                ( case st of
                    Left s -> s
                    Right s -> s
                )
                (pack $ formatTime defaultTimeLocale "%F %T %z" time)
                (pack $ fst result)
    return (st, build)
