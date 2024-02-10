{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module WebHook (Commit (..), Repo (..), runWebHook) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, handle)
import Control.Monad (void)
import Control.Monad.Error.Class (MonadError (catchError))
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
import Fmt (format)
import GHC.Generics (Generic)
import Log (LogT, Logger, defaultLogLevel, logAttention_, logInfo_, runLogT)
import Network.HTTP.Req (POST (POST), ReqBodyJson (ReqBodyJson), bsResponse, defaultHttpConfig, header, https, req, responseBody, runReq, (/:))

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

postStatus :: Text -> Commit -> State -> Text -> LogT IO ()
postStatus token commit s desc = flip catchError (logInfo_ . pack . show) $ do
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
            ""
            desc
            "se-group9/ci-server"
    headers =
        header "Accept" "application/vnd.github+json"
            <> header "Authorization" (format "token {}" token)
            <> header "X-GitHub-Api-Version" "2022-11-28"
            <> header "User-Agent" "se-group9-ci"

-- | Run webhook jobs and post status to github
runWebHook :: Logger -> Text -> Commit -> IO ()
runWebHook logger token commit =
    void $ forkIO $ runLogT "reporter" logger defaultLogLevel $ do
        postStatus token commit Pending "Working on checks..."
        postStatus token commit Success "No checks implemented yet"
