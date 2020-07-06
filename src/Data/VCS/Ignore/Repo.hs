module Data.VCS.Ignore.Repo where

import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.IO.Class         ( MonadIO )


class Repo r where

  scanRepo :: (MonadIO m, MonadThrow m) => FilePath -> m r

  isIgnored :: r -> FilePath -> Bool
