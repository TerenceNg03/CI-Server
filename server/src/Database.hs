{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Database (getBuilds, insertBuild) where

import Control.Monad.IO.Class (MonadIO)
import Database.Esqueleto.Experimental (Entity (..), from, insert, select, table)
import Database.Persist.Sql (SqlPersistT)

import Schema

-- | Get all builds from the database
getBuilds :: (MonadIO m) => SqlPersistT m [Entity Build]
getBuilds = select $ from $ table @Build

insertBuild :: (MonadIO m) => Build -> SqlPersistT m (Key Build)
insertBuild build = insert build
