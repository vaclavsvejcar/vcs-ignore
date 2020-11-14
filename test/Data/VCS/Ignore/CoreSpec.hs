{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TypeApplications  #-}

module Data.VCS.Ignore.CoreSpec
  ( spec
  )
where

import           Control.Monad.Catch            ( throwM )
import qualified Data.List                     as L
import           Data.VCS.Ignore.Core
import           Data.VCS.Ignore.PathFilter     ( PathFilter(..)
                                                , notMatched
                                                )
import           Data.VCS.Ignore.Repo           ( Repo(..)
                                                , RepoError(..)
                                                )
import           Data.VCS.Ignore.RepoPath       ( RepoPath(..) )
import           System.FilePath                ( (</>) )
import           Test.Hspec


spec :: Spec
spec = do

  describe "findRepo" $ do
    it "finds repo for some path inside repo" $ do
      let path     = testRepoRoot </> "a" </> "b"
          expected = TestRepo testRepoRoot
      findRepo path `shouldReturn` Just expected

    it "finds no repo for path outside repo" $ do
      let path = "some" </> "path"
      findRepo path `shouldReturn` Nothing @TestRepo


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
  scanRepo path | path == testRepoRoot = pure TestRepo { trPath = path }
                | otherwise            = throwM $ InvalidRepo path "err"
  isExcluded _ (RepoPath chunks) = "excluded.txt" `L.elem` chunks


testPathFilter :: PathFilter
testPathFilter = PathFilter $ \case
  rp@(RepoPath chunks) | "test-b.txt" `L.elem` chunks -> notMatched rp
  other -> pure other

testRepoRoot :: FilePath
testRepoRoot = "test-data" </> "fake-git-repo"
