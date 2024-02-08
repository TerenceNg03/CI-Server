{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds #-}

module Schema where

import           Data.Text (Text)
import           Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), withObject)
import           Data.Aeson.Types (Parser, Object, Pair)
import qualified Database.Persist.TH as PTH
import           Database.Persist (Entity(..), Entity)
import           Database.Persist.Sql (fromSqlKey, toSqlKey)


-- | Defines the schema for 'Build' entity with a unique identifier and build details.
PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  Build sql=builds
    timestamp Text
    status Text
    deriving Show Read
|]

instance ToJSON (Entity Build) where
    toJSON (Entity bid build) = object $
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
    return Build
        { buildTimestamp = bTimestamp
        , buildStatus = bStatus
        }