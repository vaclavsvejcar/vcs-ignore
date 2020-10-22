{-# LANGUAGE StrictData #-}

module Data.VCS.Ignore.Core where

import           Control.Exception              ( catch )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Data.List                     as L
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import           Data.VCS.Ignore.FileSystem     ( walkPaths )
import           Data.VCS.Ignore.PathFilter     ( PathFilter(..)
                                                , PathNotMatched
                                                )
import           Data.VCS.Ignore.Repo           ( Repo(..) )
import           Data.VCS.Ignore.RepoPath       ( RepoPath
                                                , fromFilePath
                                                )


listRepo :: (MonadIO m, Repo r) => r -> PathFilter -> m [RepoPath]
listRepo repo searchFilter = walkRepo repo searchFilter pure


walkRepo :: (MonadIO m, Repo r) => r -> PathFilter -> (RepoPath -> m a) -> m [a]
walkRepo repo (PathFilter filterFn) fn = do
  let search path | isExcluded repo path = pure Nothing
                  | otherwise            = applyFilter path >>= mapM fn
  catMaybes <$> walkPaths (repoRoot repo) (search . relativePath)
 where
  relativePath = fromFilePath . dropPrefix (repoRoot repo)
  dropPrefix prefix t = fromMaybe t (L.stripPrefix prefix t)
  applyFilter path = liftIO $ catch
    (Just <$> filterFn path)
    (\ex -> let _ = (ex :: PathNotMatched) in pure Nothing)
