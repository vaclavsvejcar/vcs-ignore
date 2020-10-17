{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.VCS.Ignore.RepoPathSpec
  ( spec
  )
where

import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.VCS.Ignore.RepoPath
import           Test.Hspec
import           Test.Toolkit                   ( matchesException )


spec :: Spec
spec = do

  describe "fromRelativePath" $ do
    it "creates RepoPath from valid path relative to the repository" $ do
      let sample   = "some/repo/path.txt"
          expected = RepoPath $ "some" :| ["repo", "path.txt"]
      fromRelativePath sample `shouldBe` Just expected

    it "fails for empty path" $ do
      let sample = ""
          check (Just (InvalidRepoPath "")) = True
          check _                           = False
      fromRelativePath sample `shouldSatisfy` matchesException check

