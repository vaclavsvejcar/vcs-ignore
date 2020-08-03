{-# LANGUAGE TypeApplications #-}

module Data.VCS.Ignore.Core where

import           Control.Monad.IO.Class         ( MonadIO )
import           Data.VCS.Ignore.FileSystem     ( walkPaths )
import           Data.VCS.Ignore.Repo           ( Repo(..) )


data SearchFilter = SearchFilter
  deriving (Eq, Show)


listRepo :: (MonadIO m, Repo r) => r -> SearchFilter -> m [FilePath]
listRepo repo searchFilter = walkRepo repo searchFilter id


walkRepo :: (MonadIO m, Repo r) => r -> SearchFilter -> (FilePath -> a) -> m [a]
walkRepo repo searchFilter fn = undefined -- TODO
