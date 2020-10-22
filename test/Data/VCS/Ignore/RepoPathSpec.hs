{-# LANGUAGE OverloadedStrings #-}

module Data.VCS.Ignore.RepoPathSpec
  ( spec
  )
where

import           Data.VCS.Ignore.RepoPath
import           Test.Hspec


spec :: Spec
spec = do

  describe "fromRelativePath" $ do
    it "creates RepoPath from valid path relative to the repository" $ do
      let sample   = "some/repo/path.txt"
          expected = RepoPath ["some", "repo", "path.txt"]
      fromRelativePath sample `shouldBe` expected

