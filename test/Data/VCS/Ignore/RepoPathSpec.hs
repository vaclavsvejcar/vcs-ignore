{-# LANGUAGE OverloadedStrings #-}

module Data.VCS.Ignore.RepoPathSpec
  ( spec
  )
where

import           Data.VCS.Ignore.RepoPath
import           Test.Hspec


spec :: Spec
spec = do

  describe "fromFilePath" $ do
    it "creates RepoPath from non-empty FilePath" $ do
      let sample   = "some/repo/path.txt"
          expected = RepoPath ["some", "repo", "path.txt"]
      fromFilePath sample `shouldBe` expected
