{-# LANGUAGE OverloadedStrings #-}

module FileLogger (withFileLogger) where

import Control.Monad.IO.Unlift
import qualified Data.Text.IO as T
import System.IO (hFlush, stdout)

import Log (Logger, mkBulkLogger, showLogMessage)
import Log.Internal.Logger (withLogger)

-- | Print log to a file
withFileLogger :: (MonadUnliftIO m) => FilePath -> (Logger -> m r) -> m r
withFileLogger file act = withRunInIO $ \unlift -> do
    logger <-
        mkBulkLogger
            "file-bulk"
            ( \msgs -> do
                mapM_ (T.appendFile file . showLogMessage Nothing) msgs
                hFlush stdout
            )
            (return ())
    withLogger logger (unlift . act)
