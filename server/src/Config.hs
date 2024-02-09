{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Config (Config (..)) where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
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
    }
    deriving (Generic)

instance FromJSON Config where
    parseJSON = withObject "Config" $ \v ->
        Config
            <$> v .: "port-number"
            <*> v .: "web-secret"
            <*> v .: "db-file"
            <*> v .: "log-file"
