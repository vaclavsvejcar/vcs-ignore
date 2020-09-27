{-# LANGUAGE StrictData #-}

module Data.VCS.Ignore.Core where

import           Control.Exception              ( catch )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Maybe                     ( catMaybes )
import           Data.VCS.Ignore.FileSystem     ( walkPaths )
import           Data.VCS.Ignore.PathFilter     ( PathFilter(..)
                                                , PathNotMatched
                                                )
import           Data.VCS.Ignore.Repo           ( Repo(..) )


listRepo :: (MonadIO m, Repo r) => r -> PathFilter -> m [FilePath]
listRepo repo searchFilter = walkRepo repo searchFilter pure


walkRepo :: (MonadIO m, Repo r) => r -> PathFilter -> (FilePath -> m a) -> m [a]
walkRepo repo (PathFilter filterFn) fn = do
  let search path | isExcluded repo path = pure Nothing
                  | otherwise            = applyFilter path >>= mapM fn
  catMaybes <$> walkPaths (repoRoot repo) search
 where
  applyFilter path = liftIO $ catch
    (Just <$> filterFn path)
    (\ex -> let _ = (ex :: PathNotMatched) in pure Nothing)
