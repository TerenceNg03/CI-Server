{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database (migrateAll, insertBuild, getBuild, getBuildByHash, getBuilds) where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Database.Esqueleto.Experimental (Entity (..), PersistEntity (Key), from, get, insert, select, table, val, where_, (==.), (^.))
import Database.Persist.Sqlite (SqlPersistT)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics (Generic)

-- | Defines the schema for 'Build' entity with a unique identifier and build details.
share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
  Build sql=builds
    commitHash Text
    date Text
    log Text
    UniqueCommitHash commitHash
    deriving Show Read Generic
|]

-- | Get all builds from the database
getBuilds :: (MonadIO m) => SqlPersistT m [Entity Build]
getBuilds = select $ from $ table @Build

-- | Get a build from the database by its unique identifier
getBuild :: (MonadIO m) => Key Build -> SqlPersistT m (Maybe Build)
getBuild = get

-- | Get a build from the database by its unique commit hash
getBuildByHash :: (MonadIO m) => Text -> SqlPersistT m (Maybe (Entity Build))
getBuildByHash hash = fmap listToMaybe $ select $ do
    builds <- from $ table @Build
    where_ (builds ^. BuildCommitHash ==. val hash)
    pure builds

-- | Insert a build into the database
insertBuild :: (MonadIO m) => Build -> SqlPersistT m (Key Build)
insertBuild = insert
