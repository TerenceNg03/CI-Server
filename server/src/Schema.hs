{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Schema where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Aeson.Types (Object, Pair, Parser)
import Data.Text (Text)
import Database.Persist (Entity (..))
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import qualified Database.Persist.TH as PTH

-- | Defines the schema for 'Build' entity with a unique identifier and build details.
PTH.share
    [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"]
    [PTH.persistLowerCase|
  Build sql=builds
    timestamp Text
    status Text
    deriving Show Read
|]

instance ToJSON (Entity Build) where
    toJSON (Entity bid build) =
        object $
            "id" .= (fromSqlKey bid) : buildPairs build

instance ToJSON Build where
    toJSON build = object (buildPairs build)

buildPairs :: Build -> [Pair]
buildPairs build =
    [ "timestamp" .= buildTimestamp build
    , "status" .= buildStatus build
    ]

instance FromJSON (Entity Build) where
    parseJSON = withObject "Build Entity" $ \o -> do
        build <- parseBuild o
        bid <- o .: "id"
        return $ Entity (toSqlKey bid) build

instance FromJSON Build where
    parseJSON = withObject "Build" parseBuild

parseBuild :: Object -> Parser Build
parseBuild o = do
    bTimestamp <- o .: "timestamp"
    bStatus <- o .: "status"
    return
        Build
            { buildTimestamp = bTimestamp
            , buildStatus = bStatus
            }
