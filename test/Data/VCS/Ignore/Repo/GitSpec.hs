{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.VCS.Ignore.Repo.GitSpec
  ( spec
  )
where

import qualified Data.List                     as L
import qualified Data.Text                     as T
import           Data.VCS.Ignore.Repo           ( RepoError(..) )
import           Data.VCS.Ignore.Repo.Git
import           System.Directory               ( makeAbsolute )
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
            , repo </> ".gitignore"
            ]
      L.sort <$> findGitIgnores repo `shouldReturn` L.sort expected


  describe "gitIgnorePatterns" $ do
    it "loads patterns for all .gitignore files in repo" $ do
      let expected =
            [("/a/", ["**/*.xml"]), ("/a/b/", ["*.txt"]), ("/", ["foo"])]
      sortFst <$> gitIgnorePatterns repo `shouldReturn` sortFst expected


  describe "scanRepo'" $ do
    it "scans repository for ignored patterns" $ do
      absRepo <- makeAbsolute repo
      let
        fn1      = pure []
        fn2      = const $ pure []
        fn3      = const $ pure True
        expected = Git
          { gitIgnoredPatterns = sortFst
            [("/", ["foo"]), ("/a/", ["**/*.xml"]), ("/a/b/", ["*.txt"])]
          , gitRepoRoot        = absRepo
          }
      result <- scanRepo' fn1 fn2 gitIgnorePatterns fn3 repo
      let result' = result
            { gitIgnoredPatterns = sortFst (gitIgnoredPatterns result)
            }
      result' `shouldBe` expected

    it "aborts scanning if given path is not valid GIT repo" $ do
      let fn1 = pure []
          fn2 = const $ pure []
          fn3 = const $ pure False
      let err (InvalidRepo _ _) = True
      scanRepo' fn1 fn2 gitIgnorePatterns fn3 repo `shouldThrow` err


  describe "isIgnored'" $ do
    it "checks whether given path is excluded" $ do
      absRepo <- makeAbsolute repo
      let git = Git
            { gitIgnoredPatterns = [ ("/"    , [])
                                   , ("/a/"  , ["**/*.xml"])
                                   , ("/a/b/", ["*.txt"])
                                   ]
            , gitRepoRoot        = absRepo
            }
      isIgnored' git "foo/bar" `shouldReturn` False
      isIgnored' git "a/hello.txt" `shouldReturn` False
      isIgnored' git "a/hello.xml" `shouldReturn` True
      isIgnored' git "a/b/hello.xml" `shouldReturn` True
      isIgnored' git "/foo/bar" `shouldReturn` False
      isIgnored' git "/a/hello.txt" `shouldReturn` False
      isIgnored' git "/a/hello.xml" `shouldReturn` True
      isIgnored' git "/a/b/hello.xml" `shouldReturn` True
      isIgnored' git "/a/b/../hello.txt" `shouldReturn` False
      isIgnored' git "/a/b/../hello.xml" `shouldReturn` True


sortFst :: Ord a => [(a, b)] -> [(a, b)]
sortFst = L.sortOn fst
