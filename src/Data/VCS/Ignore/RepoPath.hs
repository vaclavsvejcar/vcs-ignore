{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Data.VCS.Ignore.RepoPath
  ( RepoPath(..)
  , InvalidRepoPath(..)
  , fromRelativePath
  )
where

import           Control.Monad.Catch            ( Exception(..)
                                                , MonadThrow
                                                , throwM
                                                )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T


---------------------------------  DATA TYPES  ---------------------------------

newtype RepoPath = RepoPath (NonEmpty Text) deriving (Eq, Show)

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


------------------------------  PRIVATE FUNCTIONS  -----------------------------

invalidRepoPath :: MonadThrow m => FilePath -> m a
invalidRepoPath path = throwM $ InvalidRepoPath path
