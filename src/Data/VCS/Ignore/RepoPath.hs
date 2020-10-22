{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Data.VCS.Ignore.RepoPath
  ( RepoPath(..)
  , fromFilePath
  , toFilePath
  , root
  )
where

import qualified Data.List                     as L
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           System.FilePath                ( pathSeparator )


---------------------------------  DATA TYPES  ---------------------------------

newtype RepoPath = RepoPath [Text] deriving (Eq, Ord, Show)

instance Semigroup RepoPath where
  RepoPath x <> RepoPath y = RepoPath $ x <> y


------------------------------  PUBLIC FUNCTIONS  ------------------------------

fromFilePath :: FilePath -> RepoPath
fromFilePath = RepoPath . T.splitOn "/" . T.replace "\\" "/" . T.pack


toFilePath :: RepoPath -> FilePath
toFilePath (RepoPath chunks) =
  L.intercalate [pathSeparator] (T.unpack <$> chunks)


root :: RepoPath
root = RepoPath []
