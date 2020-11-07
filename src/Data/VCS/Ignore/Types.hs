{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StrictData                #-}

module Data.VCS.Ignore.Types
  ( VCSIgnoreError(..)
  , fromVCSIgnoreError
  , toVCSIgnoreError
  )
where

import           Control.Exception              ( Exception(..)
                                                , SomeException
                                                )
import           Data.Typeable                  ( cast )


---------------------------------  DATA TYPES  ---------------------------------

data VCSIgnoreError = forall e . Exception e => VCSIgnoreError e

instance Show VCSIgnoreError where
  show (VCSIgnoreError e) = show e

instance Exception VCSIgnoreError where
  displayException (VCSIgnoreError e) = displayException e


------------------------------  PUBLIC FUNCTIONS  ------------------------------

fromVCSIgnoreError :: Exception e => SomeException -> Maybe e
fromVCSIgnoreError e = do
  VCSIgnoreError e' <- fromException e
  cast e'


toVCSIgnoreError :: Exception e => e -> SomeException
toVCSIgnoreError = toException . VCSIgnoreError
