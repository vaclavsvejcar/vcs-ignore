{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Data.VCS.Ignore.PathFilter
  ( PathNotMatched(..)
  , PathFilter(..)
  , notMatched
  )
where

import           Control.Monad                  ( (>=>) )
import           Control.Monad.Catch            ( Exception(..)
                                                , MonadThrow
                                                , throwM
                                                )
import           Control.Monad.IO.Class         ( MonadIO )


data PathNotMatched = PathNotMatched FilePath
  deriving Show


instance Exception PathNotMatched where
  displayException (PathNotMatched path) =
    mconcat ["Path '", path, "' not matched"]


newtype PathFilter = PathFilter
  { unPathFilter :: forall m. (MonadIO m, MonadThrow m) => FilePath -> m FilePath
  }

instance Semigroup PathFilter where
  PathFilter x <> PathFilter y = PathFilter $ x >=> y

instance Monoid PathFilter where
  mempty = PathFilter $ \input -> pure input


notMatched :: MonadThrow m => FilePath -> m a
notMatched path = throwM (PathNotMatched path)

