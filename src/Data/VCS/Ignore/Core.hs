{-# LANGUAGE StrictData       #-}
{-# LANGUAGE TypeApplications #-}

module Data.VCS.Ignore.Core
  ( findRepo
  , listRepo
  , walkRepo
  )
where

import           Control.Exception              ( catch
                                                , try
                                                )
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
import           Data.VCS.Ignore.Types          ( VCSIgnoreError )
import           System.FilePath                ( takeDirectory )


findRepo :: (MonadIO m, Repo r) => FilePath -> m (Maybe r)
findRepo = liftIO . go
 where
  go dir = do
    let parent = takeDirectory dir
    maybeRepo <- try @VCSIgnoreError (scanRepo dir)
    case maybeRepo of
      Left _ | parent == dir -> pure Nothing
      Left  _                -> go parent
      Right repo             -> pure . Just $ repo


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
