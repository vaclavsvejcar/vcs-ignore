{-# LANGUAGE OverloadedStrings #-}

module Data.VCS.Ignore.Repo.GitSpec
  ( spec
  )
where

import qualified Data.Text                     as T
import           Data.VCS.Ignore.Repo.Git
import           Data.VCS.Ignore.RepoPath       ( RepoPath(..) )
import           System.FilePath                ( (</>) )
import           Test.Hspec


spec :: Spec
spec = do
  let repo = "test-data" </> "fake-git-repo"

  describe "parsePatterns" $ do
    it "parses glob patterns from input text (pattern per line)" $ do
      let input    = T.unlines [".cabal-sandbox/", "## comment", ".DS_Store"]
          expected = [".cabal-sandbox/", ".DS_Store"]
      parsePatterns input `shouldBe` expected


  describe "loadPatterns" $ do
    it "loads and parses glob patterns from input file" $ do
      let source   = repo </> "a" </> ".gitignore"
          expected = ["*.xml"]
      loadPatterns source `shouldReturn` expected

    it "returns empty list if input cannot be read" $ do
      let source = repo </> "non-existing"
      loadPatterns source `shouldReturn` []


  describe "findGitIgnores" $ do
    it "finds all .gitignore files in repo" $ do
      let expected =
            [ repo </> "a" </> ".gitignore"
            , repo </> "a" </> "b" </> ".gitignore"
            ]
      findGitIgnores repo `shouldReturn` expected


  describe "loadGitIgnores" $ do
    it "loads patterns for all .gitignore files in repo" $ do
      let expected =
            [(RepoPath ["a"], ["*.xml"]), (RepoPath ["a", "b"], ["*.txt"])]
      loadGitIgnores repo `shouldReturn` expected
