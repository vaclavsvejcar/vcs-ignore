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
import           Data.Maybe                     ( maybeToList )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.VCS.Ignore.FileSystem     ( findPaths
                                                , toPosixPath
                                                )
import           Data.VCS.Ignore.Repo           ( Repo(..)
                                                , RepoError(..)
                                                )
import           System.Directory               ( XdgDirectory(XdgConfig)
                                                , canonicalizePath
                                                , doesDirectoryExist
                                                , getXdgDirectory
                                                , makeAbsolute
                                                )
import           System.FilePath                ( makeRelative
                                                , (</>)
                                                )
import qualified System.FilePath.Glob          as G


---------------------------------  DATA TYPES  ---------------------------------

data Git = Git
  { gitIgnoredPatterns :: [(FilePath, [G.Pattern])]
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


gitIgnorePatterns :: MonadIO m => FilePath -> m [(FilePath, [G.Pattern])]
gitIgnorePatterns repoDir = do
  gitIgnores <- findGitIgnores repoDir
  mapM (\p -> (toPosixPath . path $ p, ) <$> loadPatterns p) gitIgnores
  where path p = stripSuffix' ".gitignore" $ stripPrefix' repoDir p


repoPatterns :: MonadIO m => FilePath -> m [G.Pattern]
repoPatterns repoDir = loadPatterns $ repoDir </> "info" </> "exclude"


globalPatterns :: MonadIO m => m [G.Pattern]
globalPatterns =
  (liftIO . getXdgDirectory XdgConfig $ ("git" </> "ignore")) >>= loadPatterns


scanRepo' :: (MonadIO m, MonadThrow m)
          => m [G.Pattern]
          -> (FilePath -> m [G.Pattern])
          -> (FilePath -> m [(FilePath, [G.Pattern])])
          -> (FilePath -> m Bool)
          -> FilePath
          -> m Git
scanRepo' globalPatternsFn repoPatternsFn gitIgnoresFn isGitRepoFn repoDir = do
  absRepoDir <- liftIO $ makeAbsolute repoDir
  gitRepo    <- isGitRepoFn absRepoDir
  (if gitRepo then proceed else abort) absRepoDir
 where
  abort repoDir' = throwM $ InvalidRepo repoDir' "not a valid GIT repository"
  proceed repoDir' = do
    globalPatterns' <- globalPatternsFn
    repoPatterns'   <- repoPatternsFn repoDir'
    gitIgnores      <- gitIgnoresFn repoDir'
    let (r, o)   = sep gitIgnores
        patterns = [("/", globalPatterns' <> repoPatterns' <> r)] <> o
    pure Git { gitIgnoredPatterns = patterns, gitRepoRoot = repoDir' }
  sep xs =
    let predicate = \(p, _) -> p == "/"
        woRoot    = filter (not . predicate) xs
        root      = concat . maybeToList $ snd <$> L.find predicate xs
    in  (root, woRoot)


isExcluded' :: MonadIO m => Git -> FilePath -> m Bool
isExcluded' git@(Git patterns _) path = do
  np <- toPosixPath <$> normalize (repoRoot git) path
  pure $ any (ignored np) (filtered np)
 where
  sanitized  = addPrefix "/"
  asRepoPath = \np -> (`stripPrefix'` sanitized np)
  ignored    = \np (prefix, ptns) -> any (`G.match` asRepoPath np prefix) ptns
  filtered   = \np -> filter (onPath np) patterns
  onPath     = \np (p, _) -> p `L.isPrefixOf` sanitized np

------------------------------  PRIVATE FUNCTIONS  -----------------------------


addPrefix :: String -> String -> String
addPrefix prefix str | prefix `L.isPrefixOf` str = str
                     | otherwise                 = prefix <> str


stripPrefix' :: String -> String -> String
stripPrefix' prefix str =
  maybe str T.unpack (T.stripPrefix (T.pack prefix) (T.pack str))


stripSuffix' :: String -> String -> String
stripSuffix' suffix str =
  maybe str T.unpack (T.stripSuffix (T.pack suffix) (T.pack str))


normalize :: MonadIO m => FilePath -> FilePath -> m FilePath
normalize repoDir path = do
  canonicalized <- liftIO . canonicalizePath $ repoDir </> stripPrefix' "/" path
  pure $ makeRelative repoDir canonicalized

