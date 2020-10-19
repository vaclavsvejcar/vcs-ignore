{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Data.VCS.Ignore.RepoPath
  ( RepoPath(..)
  , InvalidRepoPath(..)
  , fromRelativePath
  , toRelativePath
  )
where

import           Control.Monad.Catch            ( Exception(..)
                                                , MonadThrow
                                                , throwM
                                                )
import qualified Data.List                     as L
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as NEL
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           System.FilePath                ( pathSeparator )


---------------------------------  DATA TYPES  ---------------------------------

newtype RepoPath = RepoPath (NonEmpty Text) deriving (Eq, Ord, Show)

data InvalidRepoPath = InvalidRepoPath FilePath
  deriving Show

instance Exception InvalidRepoPath where
  displayException (InvalidRepoPath path) =
    mconcat ["Cannot parse path '", path, "' into RepoPath"]

instance Semigroup RepoPath where
  RepoPath x <> RepoPath y = RepoPath $ x <> y


------------------------------  PUBLIC FUNCTIONS  ------------------------------

fromRelativePath :: MonadThrow m => FilePath -> m RepoPath
fromRelativePath path = case filter (not . T.null) chunks of
  (x : xs) -> pure . RepoPath $ (x :| xs)
  []       -> invalidRepoPath path
  where chunks = T.splitOn "/" . T.replace "\\" "/" . T.pack $ path


toRelativePath :: RepoPath -> FilePath
toRelativePath (RepoPath chunks) =
  L.intercalate [pathSeparator] (T.unpack <$> NEL.toList chunks)


------------------------------  PRIVATE FUNCTIONS  -----------------------------

invalidRepoPath :: MonadThrow m => FilePath -> m a
invalidRepoPath path = throwM $ InvalidRepoPath path
