{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TupleSections       #-}

module Data.VCS.Ignore.Repo.Git
  ( Git(..)
  , isGitRepo
  , parsePatterns
  , loadPatterns
  , findGitIgnores
  , gitIgnorePatterns
  , repoPatterns
  , globalPatterns
  , scanRepo'
  , isExcluded'
  )
where

import           Control.Exception              ( SomeException
                                                , catch
                                                )
import           Control.Monad.Catch            ( MonadThrow
                                                , throwM
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Data.List                     as L
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.VCS.Ignore.FileSystem     ( findPaths )
import           Data.VCS.Ignore.Repo           ( Repo(..)
                                                , RepoError(..)
                                                )
import           Data.VCS.Ignore.RepoPath       ( RepoPath(..) )
import qualified Data.VCS.Ignore.RepoPath      as RP
import           System.Directory               ( XdgDirectory(XdgConfig)
                                                , doesDirectoryExist
                                                , getXdgDirectory
                                                )
import           System.FilePath                ( (</>) )
import qualified System.FilePath.Glob          as G


---------------------------------  DATA TYPES  ---------------------------------

data Git = Git
  { gitIgnoredPatterns :: [(RepoPath, [G.Pattern])]
  , gitRepoRoot        :: FilePath
  }
  deriving (Eq, Show)


instance Repo Git where
  repoRoot   = gitRepoRoot
  scanRepo   = scanRepo' globalPatterns repoPatterns gitIgnorePatterns isGitRepo
  isExcluded = isExcluded'


------------------------------  PUBLIC FUNCTIONS  ------------------------------

isGitRepo :: MonadIO m => FilePath -> m Bool
isGitRepo path = liftIO . doesDirectoryExist $ path </> ".git"

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


gitIgnorePatterns :: MonadIO m => FilePath -> m [(RepoPath, [G.Pattern])]
gitIgnorePatterns repoDir = do
  gitIgnores <- findGitIgnores repoDir
  mapM (\p -> (path p, ) <$> loadPatterns p) gitIgnores
 where
  path p = RP.stripSuffix (RP.fromFilePath ".gitignore")
    $ RP.stripPrefix (RP.fromFilePath repoDir) (RP.fromFilePath p)


repoPatterns :: MonadIO m => FilePath -> m [G.Pattern]
repoPatterns repoDir = loadPatterns $ repoDir </> "info" </> "exclude"


globalPatterns :: MonadIO m => m [G.Pattern]
globalPatterns =
  (liftIO . getXdgDirectory XdgConfig $ ("git" </> "ignore")) >>= loadPatterns


scanRepo' :: (MonadIO m, MonadThrow m)
          => m [G.Pattern]
          -> (FilePath -> m [G.Pattern])
          -> (FilePath -> m [(RepoPath, [G.Pattern])])
          -> (FilePath -> m Bool)
          -> FilePath
          -> m Git
scanRepo' globalPatternsFn repoPatternsFn gitIgnoresFn isGitRepoFn repoDir = do
  gitRepo <- isGitRepoFn repoDir
  if gitRepo then proceed else abort
 where
  abort   = throwM $ InvalidRepo repoDir "not a valid GIT repository"
  proceed = do
    globalPatterns' <- globalPatternsFn
    repoPatterns'   <- repoPatternsFn repoDir
    gitIgnores      <- gitIgnoresFn repoDir
    let patterns = [(RP.root, globalPatterns' <> repoPatterns')] <> gitIgnores
    pure Git { gitIgnoredPatterns = patterns, gitRepoRoot = repoDir }


isExcluded' :: Git -> RepoPath -> Bool
isExcluded' (Git patterns _) path = any ignored filtered
 where
  asFilePath = T.unpack . T.intercalate "/" . unprefixed
  unprefixed = \prefix -> unRepoPath $ RP.stripPrefix prefix path
  ignored    = \(prefix, ptns) -> any (`G.match` asFilePath prefix) ptns
  filtered   = filter onPath patterns
  onPath     = \(p, _) -> p `RP.isPrefixOf` path
