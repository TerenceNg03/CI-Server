{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Config (Config (..)) where

import Data.Aeson (
    FromJSON (parseJSON),
    Options (constructorTagModifier, fieldLabelModifier),
    camelTo2,
    defaultOptions,
    genericParseJSON,
 )
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Configuration for web server
data Config = Config
    { portNumber :: !Int
    -- ^ port number of the server
    , webSecret :: !Text
    -- ^ web secret for verify github push
    , dbFile :: !FilePath
    -- ^ database file path
    , logFile :: !FilePath
    -- ^ log file path
    , githubToken :: !Text
    -- ^ github token ("<USERNAME> <TOKEN>")
    , domain :: !Text
    -- ^ domain name and protocol used by server ("https://yourdomain.com")
    }
    deriving (Generic)

jsonOptions :: Options
jsonOptions =
    defaultOptions
        { fieldLabelModifier = camelTo2 '-'
        , constructorTagModifier = camelTo2 '-'
        }

instance FromJSON Config where
    parseJSON = genericParseJSON jsonOptions
