{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
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
import           Data.VCS.Ignore.Repo           ( Repo(..) )
import           Data.VCS.Ignore.RepoPath       ( RepoPath(..) )
import qualified Data.VCS.Ignore.RepoPath      as RP
import           System.Directory               ( XdgDirectory(XdgConfig)
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


-- TODO isExcluded
instance Repo Git where
  repoRoot   = gitRepoRoot
  scanRepo   = scanRepo' loadGlobalPatterns loadRepoPatterns loadGitIgnores
  isExcluded = isExcluded'


------------------------------  PUBLIC FUNCTIONS  ------------------------------

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


loadRepoPatterns :: MonadIO m => FilePath -> m [G.Pattern]
loadRepoPatterns repoDir = loadPatterns $ repoDir </> "info" </> "exclude"


loadGlobalPatterns :: MonadIO m => m [G.Pattern]
loadGlobalPatterns =
  (liftIO . getXdgDirectory XdgConfig $ ("git" </> "ignore")) >>= loadPatterns


scanRepo' :: MonadIO m
          => m [G.Pattern]
          -> (FilePath -> m [G.Pattern])
          -> (FilePath -> m [(RepoPath, [G.Pattern])])
          -> FilePath
          -> m Git
scanRepo' globalPatternsFn repoPatternsFn gitIgnoresFn repoDir = do
  globalPatterns <- globalPatternsFn
  repoPatterns   <- repoPatternsFn repoDir
  gitIgnores     <- gitIgnoresFn repoDir
  let patterns = [(RP.root, globalPatterns <> repoPatterns)] <> gitIgnores
  pure Git { gitIgnoredPatterns = patterns, gitRepoRoot = repoDir }


isExcluded' :: Git -> RepoPath -> Bool
isExcluded' (Git patterns _) path = any ignored filtered
 where
  asFilePath = T.unpack . T.intercalate "/" . unprefixed
  unprefixed = \prefix -> rpChunks $ RP.stripPrefix prefix path
  ignored    = \(prefix, ptns) -> any (`G.match` asFilePath prefix) ptns
  filtered   = filter onPath patterns
  onPath     = \(p, _) -> p `RP.isPrefixOf` path

{-
isExcluded' (Git patterns _) path@(RepoPath chunks) = any (`G.match` fp) ps
 where
  fp     = T.unpack . T.intercalate "/" $ chunks
  ps     = concat $ snd <$> filter onPath patterns
  onPath = \(p, _) -> p `RP.isPrefixOf` path
-}
