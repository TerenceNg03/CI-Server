{-# LANGUAGE DeriveGeneric #-}

module WebHook (Commit (..), Repo (..)) where

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
import Data.Text (Text)
import GHC.Generics (Generic)

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
