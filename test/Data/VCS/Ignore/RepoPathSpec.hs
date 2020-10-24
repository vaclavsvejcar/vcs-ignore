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
      fromFilePath "repo/path.txt" `shouldBe` RepoPath ["repo", "path.txt"]
      fromFilePath "/repo/path.txt" `shouldBe` RepoPath ["repo", "path.txt"]

    it "considers empty FilePath path as repo root" $ do
      fromFilePath "" `shouldBe` RepoPath []

    it "considers single slash as repo root" $ do
      fromFilePath "/" `shouldBe` RepoPath []
