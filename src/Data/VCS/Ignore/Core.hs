{-# LANGUAGE StrictData #-}

module Data.VCS.Ignore.Core where

import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Maybe                     ( catMaybes )
import           Data.VCS.Ignore.FileSystem     ( walkPaths )
import           Data.VCS.Ignore.Repo           ( Repo(..) )
-- import Control.Monad.Catch (catch)
import           Control.Exception              ( catch )
import           Control.Monad.Catch            ( MonadThrow )
import           Data.VCS.Ignore.PathFilter     ( FilterNotMatched
                                                , PathFilter(..)
                                                )


listRepo :: (MonadIO m, Repo r) => r -> PathFilter -> m [FilePath]
listRepo repo searchFilter = walkRepo repo searchFilter id


walkRepo :: (MonadIO m, Repo r) => r -> PathFilter -> (FilePath -> a) -> m [a]
walkRepo repo (PathFilter filterFn) fn = do
  let search path | isExcluded repo path = Nothing
                  | otherwise            = pure . fn $ path
  -- let shite = 
  paths <- catMaybes <$> walkPaths (repoPath repo) search
  -- filtered <- mapM (applyFilter filterFn) paths
  pure paths
 where
   applyFilter filterFn path = liftIO $ catch
     (Just <$> filterFn path)
     (\ex -> let _ = (ex :: FilterNotMatched) in pure Nothing)

cc :: (MonadThrow m, MonadIO m) => PathFilter -> FilePath -> m (Maybe FilePath)
cc (PathFilter filterFn) path = liftIO $ catch
  (Just <$> filterFn path)
  (\ex -> let _ = (ex :: FilterNotMatched) in pure Nothing)
