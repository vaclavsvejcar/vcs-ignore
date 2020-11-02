{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TupleSections       #-}

module Data.VCS.Ignore.Repo.Git where

import           Control.Exception              ( SomeException
                                                , catch
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Data.List                     as L
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.VCS.Ignore.FileSystem     ( findPaths )
import           Data.VCS.Ignore.RepoPath       ( RepoPath )
import qualified Data.VCS.Ignore.RepoPath      as RP
import           System.Directory               ( XdgDirectory(XdgConfig)
                                                , getXdgDirectory
                                                )
import           System.FilePath                ( (</>) )
import qualified System.FilePath.Glob          as G


data Git = Git
  { gitIgnoredPatterns :: [(RepoPath, [G.Pattern])]
  , gitRepoRoot        :: FilePath
  }
  deriving (Eq, Show)


parsePatterns :: Text -> [G.Pattern]
parsePatterns = fmap (G.compile . T.unpack) . filter (not . excluded) . T.lines
 where
  excluded line = or $ fmap ($ T.stripStart line) [comment, emptyLine]
  comment line = "#" `T.isPrefixOf` line
  emptyLine line = T.null line


loadPatterns :: MonadIO m => FilePath -> m [G.Pattern]
loadPatterns path = parsePatterns <$> liftIO content
 where
  content = catch (T.readFile path) (\(_ :: SomeException) -> pure T.empty)


findGitIgnores :: MonadIO m => FilePath -> m [FilePath]
findGitIgnores repoDir = findPaths repoDir isGitIgnore
  where isGitIgnore path = pure $ ".gitignore" `L.isSuffixOf` path


loadGitIgnores :: MonadIO m => FilePath -> m [(RepoPath, [G.Pattern])]
loadGitIgnores repoDir = do
  gitIgnores <- findGitIgnores repoDir
  mapM (\p -> (path p, ) <$> loadPatterns p) gitIgnores
 where
  path p = RP.stripSuffix (RP.fromFilePath ".gitignore")
    $ RP.stripPrefix (RP.fromFilePath repoDir) (RP.fromFilePath p)


loadRepoIgnored :: MonadIO m => FilePath -> m [G.Pattern]
loadRepoIgnored repoDir = loadPatterns $ repoDir </> "info" </> "exclude"


loadGlobalIgnored :: MonadIO m => m [G.Pattern]
loadGlobalIgnored =
  (liftIO . getXdgDirectory XdgConfig $ ("git" </> "ignore")) >>= loadPatterns
