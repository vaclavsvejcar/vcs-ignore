{-# LANGUAGE StrictData #-}

module Data.VCS.Ignore.Repo
  ( Repo(..)
  , RepoError(..)
  )
where

import           Control.Exception              ( Exception(..) )
import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.VCS.Ignore.Types          ( fromVCSIgnoreError
                                                , toVCSIgnoreError
                                                )


class Repo r where

  repoRoot :: r -> FilePath

  scanRepo :: (MonadIO m, MonadThrow m) => FilePath -> m r

  isExcluded :: r -> FilePath -> Bool


data RepoError = InvalidRepo FilePath Text
  deriving (Eq, Show)

instance Exception RepoError where
  displayException = displayException'
  fromException    = fromVCSIgnoreError
  toException      = toVCSIgnoreError


displayException' :: RepoError -> String
displayException' (InvalidRepo path reason) =
  mconcat ["Path '", path, "' is not a valid repository: ", T.unpack reason]
