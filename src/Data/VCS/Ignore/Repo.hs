module Data.VCS.Ignore.Repo
  ( Repo(..)
  )
where

import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.IO.Class         ( MonadIO )


class Repo r where

  repoRoot :: r -> FilePath

  scanRepo :: (MonadIO m, MonadThrow m) => FilePath -> m r

  isExcluded :: r -> FilePath -> Bool

