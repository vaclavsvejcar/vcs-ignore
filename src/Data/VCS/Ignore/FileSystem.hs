{-# LANGUAGE MultiWayIf #-}
module Data.VCS.Ignore.FileSystem
  ( findFiles
  , listFiles
  )
where


import           Control.Monad                  ( forM )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           System.Directory               ( doesDirectoryExist
                                                , doesFileExist
                                                , getDirectoryContents
                                                )
import           System.FilePath                ( (</>) )


-- | Recursively finds files on given path whose filename matches the predicate.
findFiles :: MonadIO m
          => FilePath
          -- ^ path to search
          -> (FilePath -> Bool)
          -- ^ predicate to match filename
          -> m [FilePath]
          -- ^ found files
findFiles path predicate = fmap (filter predicate) (listFiles path)


-- | Recursively find all files on given path. If file reference is passed
-- instead of directory, such file path is returned.
listFiles :: MonadIO m
          => FilePath
          -- ^ path to search
          -> m [FilePath]
          -- ^ list of found files
listFiles fileOrDir = do
  isDir  <- liftIO $ doesDirectoryExist fileOrDir
  isFile <- liftIO $ doesFileExist fileOrDir
  if
    | isDir     -> listDirectory fileOrDir
    | isFile    -> pure [fileOrDir]
    | otherwise -> pure []
 where
  listDirectory dir = do
    names <- liftIO $ getDirectoryContents dir
    paths <- forM (filter (`notElem` [".", ".."]) names) $ \name -> do
      let path = dir </> name
      isDirectory <- liftIO $ doesDirectoryExist path
      if isDirectory then listFiles path else pure [path]
    pure $ concat paths
