{-# LANGUAGE MultiWayIf #-}
module Data.VCS.Ignore.FileSystem
  ( findPaths
  , listPaths
  , walkPaths
  )
where


import           Control.Monad                  ( forM )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Maybe                     ( catMaybes )
import           System.Directory               ( doesDirectoryExist
                                                , doesFileExist
                                                , getDirectoryContents
                                                )
import           System.FilePath                ( (</>) )


-- | Recursively finds paths on given path whose filename matches the predicate.
findPaths :: MonadIO m
          => FilePath
          -- ^ path to traverse
          -> (FilePath -> Bool)
          -- ^ predicate to match filename
          -> m [FilePath]
          -- ^ list of found paths
findPaths entryPath predicate = catMaybes <$> walkPaths entryPath process
  where process path = if predicate path then Just path else Nothing


-- | Recursively finds all paths on given path. If file reference is passed
-- instead of directory, such path is returned.
listPaths :: MonadIO m
          => FilePath
          -- ^ path to traverse
          -> m [FilePath]
          -- ^ list of found paths
listPaths entryPath = walkPaths entryPath id


-- | Recursively walks the given path and performs selected action for each
-- found subpath. Output of this function is:
--
--   * If the given __path is file__, only this single path is processed and
--     returned.
--   * If the given __path is directory__, all subdirectories and files are
--     recursively processed and returned.
--   * If the given __path doesn't exist__, empy list will be returned.
walkPaths :: MonadIO m
          => FilePath
          -- ^ path to traverse
          -> (FilePath -> a)
          -- ^ function to process path
          -> m [a]
          -- ^ result of traversed & processed paths
walkPaths entryPath fn = do
  isDir  <- liftIO $ doesDirectoryExist entryPath
  isFile <- liftIO $ doesFileExist entryPath
  if
    | isDir     -> (fn entryPath :) <$> listDirectory entryPath
    | isFile    -> pure [fn entryPath]
    | otherwise -> pure []
 where
  listDirectory dir = do
    names <- liftIO $ getDirectoryContents dir
    paths <- forM (filter (`notElem` [".", ".."]) names) $ \name -> do
      let path = dir </> name
      isDirectory <- liftIO $ doesDirectoryExist path
      if isDirectory then walkPaths path fn else pure [fn path]
    pure $ concat paths
