{-# LANGUAGE OverloadedStrings #-}

module Data.VCS.Ignore.RepoPathSpec
  ( spec
  )
where

import           Data.VCS.Ignore.RepoPath
import qualified Data.VCS.Ignore.RepoPath      as RP
import           Test.Hspec


spec :: Spec
spec = do

  describe "fromFilePath" $ do
    it "creates RepoPath from non-empty FilePath" $ do
      fromFilePath "repo/path.txt" `shouldBe` RepoPath ["repo", "path.txt"]
      fromFilePath "/repo/path.txt" `shouldBe` RepoPath ["repo", "path.txt"]

    it "considers empty FilePath path as repo root" $ do
      fromFilePath "" `shouldBe` RP.root

    it "considers single slash as repo root" $ do
      fromFilePath "/" `shouldBe` RP.root


  describe "isPrefixOf" $ do
    it "detects that first RepoPath is prefix of second one" $ do
      let path1 = RepoPath ["a", "b"]
          path2 = RepoPath ["a", "b", "c", "d.txt"]
      isPrefixOf path1 path2 `shouldBe` True

    it "detects that first RepoPath is not prefix of second one" $ do
      let path1 = RepoPath ["a", "b", "e"]
          path2 = RepoPath ["a", "b", "c", "d.txt"]
      isPrefixOf path1 path2 `shouldBe` False

    it "works for empty paths" $ do
      isPrefixOf RP.root RP.root `shouldBe` True


  describe "stripPrefix" $ do
    it "strips prefix from RepoPath" $ do
      let prefix   = RepoPath ["a", "b"]
          path     = RepoPath ["a", "b", "c", "d.txt"]
          expected = RepoPath ["c", "d.txt"]
      stripPrefix prefix path `shouldBe` expected

    it "returns original paths if prefix is root RepoPath" $ do
      let prefix = RP.root
          path   = RepoPath ["a", "b", "c", "d.txt"]
      stripPrefix prefix path `shouldBe` path


  describe "stripSuffix" $ do
    it "strips suffix from RepoPath" $ do
      let suffix   = RepoPath ["c", "d.txt"]
          path     = RepoPath ["a", "b", "c", "d.txt"]
          expected = RepoPath ["a", "b"]
      stripSuffix suffix path `shouldBe` expected

    it "returns original path is suffix is empty" $ do
      let suffix = RP.root
          path   = RepoPath ["a", "b", "c", "d.txt"]
      stripSuffix suffix path `shouldBe` path
