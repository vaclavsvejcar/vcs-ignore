{-# LANGUAGE StrictData #-}
module Data.VCS.Ignore.Repo.Git where

import           Control.Monad.Catch            ( MonadThrow )
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


data Git = Git
  { gitIgnoredPatterns :: Map FilePath [String]
  }
  deriving Show


instance Repo Git where

  scanRepo = scanRepo'


-- TODO
-- [ ] load .gitignore
-- [ ] load <REPO>/info/exclude
-- [ ] load <XDG_CONFIG_HOME>/git/ignore

repoGitIgnore :: FilePath -> FilePath
repoGitIgnore repoDir = repoDir </> "info" </> "exclude"


dotGitIgnores :: MonadIO m => FilePath -> m [FilePath]
dotGitIgnores repoDir = findPaths repoDir isGitIgnore
  where isGitIgnore path = pure $ ".gitignore" `L.isSuffixOf` path


globalGitIgnore :: MonadIO m => m FilePath
globalGitIgnore = liftIO . getXdgDirectory XdgConfig $ ("git" </> "ignore")


ignoredPatterns :: MonadIO m
                => Maybe FilePath
                -> FilePath
                -> m (FilePath, [String])
ignoredPatterns targetPath filePath = do
  handle   <- liftIO $ openFile filePath ReadMode
  contents <- liftIO $ hGetContents handle
  pure (fromMaybe filePath targetPath, lines contents)


scanRepo' :: MonadIO m => FilePath -> m Git
scanRepo' repoDir = do
  dotGitIgnores' <- dotGitIgnores repoDir >>= mapM (ignoredPatterns Nothing)
  repoGitIgnore' <- ignoredPatterns Nothing $ repoGitIgnore repoDir
  globalIgnore'  <- globalGitIgnore >>= ignoredPatterns (Just "/")
  let patterns = [globalIgnore'] <> [repoGitIgnore'] <> dotGitIgnores'
  pure $ Git { gitIgnoredPatterns = M.fromList patterns }
