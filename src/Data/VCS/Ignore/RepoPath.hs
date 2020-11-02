{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Data.VCS.Ignore.RepoPath
  ( RepoPath(..)
  , root
  , fromFilePath
  , isPrefixOf
  , stripPrefix
  , stripSuffix
  )
where

import           Data.List                      ( (\\) )
import qualified Data.List                     as L
import           Data.Text                      ( Text )
import qualified Data.Text                     as T


---------------------------------  DATA TYPES  ---------------------------------

newtype RepoPath = RepoPath [Text] deriving (Eq, Ord, Show)

instance Semigroup RepoPath where
  RepoPath x <> RepoPath y = RepoPath $ x <> y


------------------------------  PUBLIC FUNCTIONS  ------------------------------

root :: RepoPath
root = RepoPath []

fromFilePath :: FilePath -> RepoPath
fromFilePath = RepoPath . chunks
 where
  chunks      = filterEmpty . T.splitOn "/" . T.replace "\\" "/" . T.pack
  filterEmpty = L.filter (not . T.null)


isPrefixOf :: RepoPath -> RepoPath -> Bool
isPrefixOf (RepoPath x) (RepoPath y) = x `L.isPrefixOf` y


stripPrefix :: RepoPath -> RepoPath -> RepoPath
stripPrefix (RepoPath prefix) (RepoPath path) = RepoPath $ path \\ prefix


stripSuffix :: RepoPath -> RepoPath -> RepoPath
stripSuffix (RepoPath []    ) path            = path
stripSuffix (RepoPath suffix) (RepoPath path) = RepoPath $ take count path
  where count = length path - length suffix
