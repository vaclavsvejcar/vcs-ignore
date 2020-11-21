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
import           Data.VCS.Ignore.FileSystem     ( walkFiles )
import           Data.VCS.Ignore.Repo           ( Repo(..) )
import           Data.VCS.Ignore.Types          ( VCSIgnoreError )
import           System.FilePath                ( takeDirectory )


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
  let search path | isExcluded repo path = pure Nothing
                  | otherwise            = Just <$> fn path
  catMaybes <$> walkFiles root' (search . relativePath)
 where
  root         = repoRoot repo
  root'        = if "/" `L.isSuffixOf` root then root else root <> "/"
  relativePath = dropPrefix root'
  dropPrefix   = \prefix t -> fromMaybe t (L.stripPrefix prefix t)
