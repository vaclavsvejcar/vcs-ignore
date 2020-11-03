{-# LANGUAGE OverloadedStrings #-}

module Data.VCS.Ignore.Repo.GitSpec
  ( spec
  )
where

import qualified Data.Text                     as T
import           Data.VCS.Ignore.Repo.Git
import           Data.VCS.Ignore.RepoPath       ( RepoPath(..) )
import qualified Data.VCS.Ignore.RepoPath      as RP
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
          expected = ["**/*.xml"]
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
            [(RepoPath ["a"], ["**/*.xml"]), (RepoPath ["a", "b"], ["*.txt"])]
      loadGitIgnores repo `shouldReturn` expected


  describe "scanRepo'" $ do
    it "scans repository for ignored patterns" $ do
      let fn1      = pure []
          fn2      = const $ pure []
          expected = Git
            { gitIgnoredPatterns = [ (RP.root            , [])
                                   , (RepoPath ["a"]     , ["**/*.xml"])
                                   , (RepoPath ["a", "b"], ["*.txt"])
                                   ]
            , gitRepoRoot        = repo
            }
      scanRepo' fn1 fn2 loadGitIgnores repo `shouldReturn` expected


  describe "isExcluded'" $ do
    it "checks whether given RepoPath is excluded" $ do
      let git = Git
            { gitIgnoredPatterns = [ (RP.root            , [])
                                   , (RepoPath ["a"]     , ["**/*.xml"])
                                   , (RepoPath ["a", "b"], ["*.txt"])
                                   ]
            , gitRepoRoot        = repo
            }
      isExcluded' git (RepoPath ["foo", "bar"]) `shouldBe` False
      isExcluded' git (RepoPath ["a", "hello.txt"]) `shouldBe` False
      isExcluded' git (RepoPath ["a", "hello.xml"]) `shouldBe` True
      isExcluded' git (RepoPath ["a", "b", "hello.xml"]) `shouldBe` True
