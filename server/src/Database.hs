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

module Database (migrateAll, insertBuild, getBuildByUUID, getBuilds) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Database.Esqueleto.Experimental (Entity (..), from, insert, select, table, val, where_, (==.), (^.))
import Database.Persist.Sqlite (SqlPersistT)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics (Generic)

-- | Defines the schema for 'Build' entity with a unique identifier and build details.
share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
  Build sql=builds
    uuid Text sqltype=UUID 
    commitHash Text
    date Text
    log Text
    Primary uuid
    deriving Show Read Generic
|]

-- | Get all builds from the database
getBuilds :: (MonadIO m) => SqlPersistT m [Entity Build]
getBuilds = select $ from $ table @Build

-- | Get a build from the database by its unique commit hash
getBuildByUUID :: (MonadIO m) => Text -> SqlPersistT m (Maybe (Entity Build))
getBuildByUUID uuid = fmap listToMaybe $ select $ do
    builds <- from $ table @Build
    where_ (builds ^. BuildUuid ==. val uuid)
    pure builds

-- | Insert a build into the database
insertBuild :: (MonadIO m) => Build -> SqlPersistT m ()
insertBuild build = void $ insert build