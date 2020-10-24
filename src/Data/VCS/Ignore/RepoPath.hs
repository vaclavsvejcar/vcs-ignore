{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Data.VCS.Ignore.RepoPath
  ( RepoPath(..)
  , fromFilePath
  )
where

import qualified Data.List                     as L
import           Data.Text                      ( Text )
import qualified Data.Text                     as T


---------------------------------  DATA TYPES  ---------------------------------

newtype RepoPath = RepoPath [Text] deriving (Eq, Ord, Show)

instance Semigroup RepoPath where
  RepoPath x <> RepoPath y = RepoPath $ x <> y


------------------------------  PUBLIC FUNCTIONS  ------------------------------

fromFilePath :: FilePath -> RepoPath
fromFilePath = RepoPath . chunks
 where
  chunks      = filterEmpty . T.splitOn "/" . T.replace "\\" "/" . T.pack
  filterEmpty = L.filter (not . T.null)
