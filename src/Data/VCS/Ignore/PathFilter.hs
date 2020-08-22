{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}


module Data.VCS.Ignore.PathFilter where

import           Control.Monad                  ( (>=>) )
import           Control.Monad.Catch            ( Exception(..)
                                                , MonadThrow
                                                , throwM
                                                )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T


data FilterNotMatched = FilterNotMatched FilePath Text
  deriving Show

instance Exception FilterNotMatched where
  displayException (FilterNotMatched path reason) =
    mconcat ["Path '", path, "' not matched, reason: ", T.unpack reason]


newtype PathFilter = PathFilter
  { unPathFilter :: forall m. (MonadIO m, MonadThrow m) => FilePath -> m FilePath
  }

instance Semigroup PathFilter where
  PathFilter pf1 <> PathFilter pf2 = PathFilter $ pf1 >=> pf2

instance Monoid PathFilter where
  mempty = PathFilter $ \input -> pure input


notMatched :: MonadThrow m => FilePath -> Text -> m a
notMatched path reason = throwM (FilterNotMatched path reason)



