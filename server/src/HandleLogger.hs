{-# LANGUAGE OverloadedStrings #-}

module HandleLogger (withHandleLogger) where

import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import qualified Data.Text.IO as T
import System.IO (Handle)

import qualified GHC.IO.Handle as T
import Log (Logger, mkBulkLogger, showLogMessage)
import Log.Internal.Logger (withLogger)

-- | Print log to a handle with flushing on each write
withHandleLogger :: (MonadUnliftIO m) => Handle -> (Logger -> m r) -> m r
withHandleLogger handle act = withRunInIO $ \unlift -> do
    logger <-
        mkBulkLogger
            "file-bulk"
            ( \msgs -> do
                mapM_ (T.hPutStrLn handle . showLogMessage Nothing) msgs
                T.hFlush handle
            )
            (return ())
    withLogger logger (unlift . act)
