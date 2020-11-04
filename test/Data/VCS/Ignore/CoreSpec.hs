{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TypeApplications  #-}

module Data.VCS.Ignore.CoreSpec
  ( spec
  )
where

import qualified Data.List                     as L
import           Data.VCS.Ignore.Core
import           Data.VCS.Ignore.PathFilter     ( PathFilter(..)
                                                , notMatched
                                                )
import           Data.VCS.Ignore.Repo           ( Repo(..) )
import           Data.VCS.Ignore.RepoPath       ( RepoPath(..) )
import           System.FilePath                ( (</>) )
import           Test.Hspec


spec :: Spec
spec = do
  let testRepoRoot = "test-data" </> "fake-git-repo"

  describe "listRepo" $ do
    it "lists repository paths, based on the search filter" $ do
      let pathFilter = mempty
          expected =
            [ RepoPath []
            , RepoPath ["a"]
            , RepoPath ["a", ".gitignore"]
            , RepoPath ["a", "b"]
            , RepoPath ["a", "b", ".gitignore"]
            , RepoPath ["a", "b", "test-b.txt"]
            , RepoPath ["a", "b", "test-b.xml"]
            , RepoPath ["a", "test-a.txt"]
            , RepoPath ["a", "test-a.xml"]
            ]
      repo   <- scanRepo @TestRepo testRepoRoot
      result <- listRepo repo pathFilter
      L.sort result `shouldBe` L.sort expected

    it "walks repository paths, based on the search filter" $ do
      let pathFilter = testPathFilter
          expected =
            [ RepoPath []
            , RepoPath ["a"]
            , RepoPath ["a", ".gitignore"]
            , RepoPath ["a", "b"]
            , RepoPath ["a", "b", ".gitignore"]
            , RepoPath ["a", "b", "test-b.xml"]
            , RepoPath ["a", "test-a.txt"]
            , RepoPath ["a", "test-a.xml"]
            ]
      repo   <- scanRepo @TestRepo testRepoRoot
      result <- listRepo repo pathFilter
      L.sort result `shouldBe` L.sort expected


data TestRepo = TestRepo
  { trPath :: FilePath
  }
  deriving (Eq, Show)

instance Repo TestRepo where
  repoRoot TestRepo {..} = trPath
  scanRepo path = pure TestRepo { trPath = path }
  isExcluded _ (RepoPath chunks) = "excluded.txt" `L.elem` chunks


testPathFilter :: PathFilter
testPathFilter = PathFilter $ \case
  rp@(RepoPath chunks) | "test-b.txt" `L.elem` chunks -> notMatched rp
  other -> pure other
