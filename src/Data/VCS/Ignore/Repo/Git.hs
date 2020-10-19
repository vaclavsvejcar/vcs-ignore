{-# LANGUAGE StrictData #-}
module Data.VCS.Ignore.Repo.Git where

import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Data.List                     as L
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import           Data.VCS.Ignore.FileSystem     ( findPaths )
import           Data.VCS.Ignore.Repo           ( Repo(..) )
import           System.Directory               ( XdgDirectory(XdgConfig)
                                                , getXdgDirectory
                                                )
import           System.FilePath                ( (</>) )
import           System.IO                      ( IOMode(ReadMode)
                                                , hGetContents
                                                , openFile
                                                )
import           System.IO.Error                ( tryIOError )


data Git = Git
  { gitIgnoredPatterns :: Map FilePath [String]
  , gitRepoRoot        :: FilePath
  }
  deriving (Eq, Show)


instance Repo Git where
  scanRepo = scanRepo' globalGitIgnore repoGitIgnore dotGitIgnores


repoGitIgnore :: FilePath -> FilePath
repoGitIgnore repoDir = repoDir </> "info" </> "exclude"


dotGitIgnores :: MonadIO m => FilePath -> m [FilePath]
dotGitIgnores repoDir = findPaths repoDir isGitIgnore
  where isGitIgnore path = pure $ ".gitignore" `L.isSuffixOf` path


globalGitIgnore :: MonadIO m => m FilePath
globalGitIgnore = liftIO . getXdgDirectory XdgConfig $ ("git" </> "ignore")


-- TODO replace generic FilePath with something more type safe
ignoredPatterns :: MonadIO m
                => Maybe FilePath
                -> FilePath
                -> m (FilePath, [String])
ignoredPatterns targetPath filePath = do
  content <- liftIO $ tryIOError loadContent
  pure $ either (const (resultPath, [])) id content
 where
  resultPath  = fromMaybe filePath targetPath
  loadContent = do
    handle   <- liftIO $ openFile filePath ReadMode
    contents <- liftIO $ hGetContents handle
    pure (resultPath, lines contents)


scanRepo' :: MonadIO m
          => m FilePath
          -> (FilePath -> FilePath)
          -> (FilePath -> m [FilePath])
          -> FilePath
          -> m Git
scanRepo' globalGitIgnoreFn repoGitIgnoreFn dotGitIgnoresFn repoDir = do
  dotGitIgnores' <- dotGitIgnoresFn repoDir >>= mapM (ignoredPatterns Nothing)
  repoGitIgnore' <- ignoredPatterns (Just "/") $ repoGitIgnoreFn repoDir
  globalIgnore'  <- globalGitIgnoreFn >>= ignoredPatterns (Just "/")
  let patterns = [globalIgnore'] <> [repoGitIgnore'] <> dotGitIgnores'
  pure $ Git { gitIgnoredPatterns = M.fromList patterns, gitRepoRoot = repoDir }
