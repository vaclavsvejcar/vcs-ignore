{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TupleSections       #-}

{-|
Module      : Data.VCS.Ignore.Repo.Git
Description : Implementation of 'Repo' for /GIT/
Copyright   : (c) 2020-2021 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

This module contains implementation of 'Repo' /type class/ for the /GIT/ content
versioning system. Most of the public functions is exported only to make them
visible for tests, end user of this library really shouldn't need to use them.
-}

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
  , isIgnored'
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

-- | Data type representing scanned instance of /GIT/ repository.
data Git = Git
  { gitIgnoredPatterns :: [(FilePath, [G.Pattern])]
  -- ^ patterns ignored at given repository paths
  , gitRepoRoot        :: FilePath
  -- ^ absolute path to the repository root
  }
  deriving (Eq, Show)


instance Repo Git where
  repoRoot  = gitRepoRoot
  scanRepo  = scanRepo' globalPatterns repoPatterns gitIgnorePatterns isGitRepo
  isIgnored = isIgnored'


------------------------------  PUBLIC FUNCTIONS  ------------------------------

-- | Checks whether given directory path is valid /GIT/ repository.
isGitRepo :: MonadIO m
          => FilePath
          -- ^ path to the directory to check
          -> m Bool
          -- ^ @True@ if the given directory is valid /GIT/ repository
isGitRepo path = liftIO . doesDirectoryExist $ path </> ".git"


-- | Parses /Glob/ patterns from given text source. Each line in input text is
-- considered to be single pattern. Lines starting with @#@ (comments) and blank
-- lines are skipped.
--
-- >>> parsePatterns "*.xml\n.DS_Store"
-- [compile "*.xml",compile ".DS_Store"]
parsePatterns :: Text
              -- ^ text to parse
              -> [G.Pattern]
              -- ^ parsed patterns
parsePatterns = fmap (G.compile . T.unpack) . filter (not . excluded) . T.lines
 where
  excluded line = or $ fmap ($ T.stripStart line) [comment, emptyLine]
  comment line = "#" `T.isPrefixOf` line
  emptyLine line = T.null line


-- | Loads /Glob/ patterns from given text file. If the fille cannot be read for
-- any reason, empty list is returned. See 'parsePatterns' for more details
-- about parsing.
loadPatterns :: MonadIO m
             => FilePath
             -- ^ path to text file to parse
             -> m [G.Pattern]
             -- ^ parsed /Glob/ patterns
loadPatterns path = parsePatterns <$> liftIO content
 where
  content = catch (T.readFile path) (\(_ :: SomeException) -> pure T.empty)


-- | Recursively finds all @.gitignore@ files within the given directory path.
findGitIgnores :: MonadIO m
               => FilePath
               -- ^ path to the directory to search in
               -> m [FilePath]
               -- ^ paths of found @.gitignore@ files
findGitIgnores repoDir = findPaths repoDir isGitIgnore
  where isGitIgnore path = pure $ ".gitignore" `L.isSuffixOf` path


-- | Recursively finds all @.gitignore@ files within the given directory path
-- and parses them into /Glob/ patterns. See 'loadPatterns' and 'findGitIgnores'
-- for more details.
gitIgnorePatterns :: MonadIO m
                  => FilePath
                  -- ^ path to the directory to search @.gitignore@ files in
                  -> m [(FilePath, [G.Pattern])]
                  -- ^ list of @.gitignore@ paths and parsed /Glob/ patterns
gitIgnorePatterns repoDir = do
  gitIgnores <- findGitIgnores repoDir
  mapM (\p -> (toPosixPath . path $ p, ) <$> loadPatterns p) gitIgnores
  where path p = stripSuffix' ".gitignore" $ stripPrefix' repoDir p


-- | Loads /GIT/ repository specific ignore patterns, present in
-- @REPO_ROOT\/info\/exclude@ file.
repoPatterns :: MonadIO m
             => FilePath
             -- ^ path to the /GIT/ repository root
             -> m [G.Pattern]
             -- ^ parsed /Glob/ patterns
repoPatterns repoDir = loadPatterns $ repoDir </> "info" </> "exclude"


-- | Loads global /GIT/  ignore patterns, present in
-- @XDG_CONFIG_GOME\/git\/ignore@ file.
globalPatterns :: MonadIO m => m [G.Pattern]
               -- ^ parsed /Glob/ patterns
globalPatterns =
  (liftIO . getXdgDirectory XdgConfig $ ("git" </> "ignore")) >>= loadPatterns


-- | Internal version of 'scanRepo', where individual functions needs to be
-- explicitly provided, which is useful mainly for testing purposes.
scanRepo' :: (MonadIO m, MonadThrow m)
          => m [G.Pattern]
          -- ^ reference to 'globalPatterns' function (or similar)
          -> (FilePath -> m [G.Pattern])
          -- ^ reference to 'repoPatterns' function (or similar)
          -> (FilePath -> m [(FilePath, [G.Pattern])])
          -- ^ reference to 'gitIgnorePatterns' function (or similar)
          -> (FilePath -> m Bool)
          -- ^ reference to 'isGitRepo' function (or similar)
          -> FilePath
          -- ^ path to /GIT/ repository root
          -> m Git
          -- ^ scanned /Git/ repository
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


-- | Internal version of 'isIgnored' function.
isIgnored' :: MonadIO m
           => Git
           -- ^ scanned /GIT/ repository
           -> FilePath
           -- ^ path to check if ignored
           -> m Bool
           -- @True@ if given path is ignored
isIgnored' git@(Git patterns _) path = do
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
  isDir         <- liftIO $ doesDirectoryExist canonicalized
  let suffix = if isDir || "/" `L.isSuffixOf` path then "/" else ""
  pure $ makeRelative repoDir canonicalized <> suffix

