{-# LANGUAGE StrictData       #-}
{-# LANGUAGE TypeApplications #-}

module Data.VCS.Ignore.Core
  ( findRepo
  , listRepo
  , walkRepo
  )
where

import           Control.Exception              ( try )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Data.List                     as L
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import           Data.VCS.Ignore.FileSystem     ( walkPaths )
import           Data.VCS.Ignore.Repo           ( Repo(..) )
import           Data.VCS.Ignore.Types          ( VCSIgnoreError )
import           System.FilePath                ( pathSeparator
                                                , takeDirectory
                                                )


findRepo :: (MonadIO m, Repo r) => FilePath -> m (Maybe r)
findRepo = liftIO . go
 where
  go dir = do
    let parent = takeDirectory dir
    maybeRepo <- try @VCSIgnoreError (scanRepo dir)
    case maybeRepo of
      Left _ | parent == dir -> pure Nothing
      Left  _                -> go parent
      Right repo             -> pure . Just $ repo


listRepo :: (MonadIO m, Repo r) => r -> m [FilePath]
listRepo repo = walkRepo repo pure


walkRepo :: (MonadIO m, Repo r) => r -> (FilePath -> m a) -> m [a]
walkRepo repo fn = do
  let search path | L.null path = pure Nothing
                  | otherwise   = doSearch path
  catMaybes <$> walkPaths root' (search . relativePath)
 where
  ps           = [pathSeparator]
  root         = repoRoot repo
  root'        = if ps `L.isSuffixOf` root then root else root <> ps
  relativePath = dropPrefix root'
  dropPrefix   = \prefix t -> fromMaybe t (L.stripPrefix prefix t)
  doSearch     = \path -> isExcluded repo path >>= process path
  process      = \path x -> if x then pure Nothing else Just <$> fn path
