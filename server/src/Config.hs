{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config (Config (..)) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))

data Config = Config
    { portNumber :: !Int
    , webSecret :: !Text
    , dbFile :: !FilePath
    } deriving Generic

instance FromJSON Config where
    parseJSON = withObject "Config" $ \v -> Config
        <$> v .: "port-number"
        <*> v .: "web-secret"
        <*> v .: "db-file"
