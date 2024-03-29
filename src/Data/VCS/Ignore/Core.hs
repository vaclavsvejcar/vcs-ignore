{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Data.VCS.Ignore.Core
-- Description : Core operations over the repository
-- Copyright   : (c) 2020-2022 Vaclav Svejcar
-- License     : BSD-3-Clause
-- Maintainer  : vaclav.svejcar@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains core operations you can perform over the scanned 'Repo'.
module Data.VCS.Ignore.Core (
    findRepo
    , listRepo
    , walkRepo
) where

import Control.Exception (try)
import Control.Monad.IO.Class (
    MonadIO
    , liftIO
 )
import qualified Data.List as L
import Data.Maybe (
    catMaybes
    , fromMaybe
 )
import Data.VCS.Ignore.FileSystem (walkPaths)
import Data.VCS.Ignore.Repo (Repo (..))
import Data.VCS.Ignore.Types (VCSIgnoreError)
import System.FilePath (
    pathSeparator
    , takeDirectory
 )

-- | Attempts to find (and scan via 'scanRepo') repository at given path.
-- If given path doesn't contain valid repository, it recursively tries in every
-- parent directory until the root directory (e.g. @C:@ or @/@) is reached.
findRepo ::
    (MonadIO m, Repo r) =>
    -- | path where to start scanning
    FilePath ->
    -- | scanned 'Repo' (if found)
    m (Maybe r)
findRepo = liftIO . go
  where
    go dir = do
        let parent = takeDirectory dir
        maybeRepo <- try @VCSIgnoreError (scanRepo dir)
        case maybeRepo of
            Left _ | parent == dir -> pure Nothing
            Left _ -> go parent
            Right repo -> pure . Just $ repo

-- | Resursively lists all non-ignored paths withing the given repository
-- (both files and directories).
listRepo ::
    (MonadIO m, Repo r) =>
    -- | repository to list
    r ->
    -- | list of non-ignored paths within the repository
    m [FilePath]
listRepo repo = walkRepo repo pure

-- | Similar to 'listRepo', but allows to perform any action on every
-- non-ignored path within the repository.
walkRepo ::
    (MonadIO m, Repo r) =>
    -- | repository to walk
    r ->
    -- | action to do on every non-excluded filepath
    (FilePath -> m a) ->
    -- | list of transformed paths
    m [a]
walkRepo repo fn = do
    let search path
            | L.null path = pure Nothing
            | otherwise = doSearch path
    catMaybes <$> walkPaths root' (search . relativePath)
  where
    ps = [pathSeparator]
    root = repoRoot repo
    root' = if ps `L.isSuffixOf` root then root else root <> ps
    relativePath = dropPrefix root'
    dropPrefix = \prefix t -> fromMaybe t (L.stripPrefix prefix t)
    doSearch = \path -> isIgnored repo path >>= process path
    process = \path x -> if x then pure Nothing else Just <$> fn path
