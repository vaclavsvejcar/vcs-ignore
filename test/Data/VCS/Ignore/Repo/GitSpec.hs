module Data.VCS.Ignore.Repo.GitSpec
  ( spec
  )
where


import           Data.VCS.Ignore.Repo.Git
import           System.FilePath                ( (</>) )
import           Test.Hspec


spec :: Spec
spec = do
  let repo = "test-data" </> "fake-git-repo"


  describe "scanRepo" $ do
    it "searches for all required info in given GIT repo" $ do
      pending

  describe "repoGitIgnore" $ do
    it "returns the path to '<REPO>/info/exclude' file" $ do
      let expected = repo </> "info" </> "exclude"
          result   = repoGitIgnore repo
      result `shouldBe` expected

  describe "dotGitIgnores" $ do
    it "recursively finds all '.gitignore' files in repository" $ do
      result <- dotGitIgnores repo
      let expected =
            [ "test-data/fake-git-repo/a/.gitignore"
            , "test-data/fake-git-repo/a/b/.gitignore"
            ]
      result `shouldBe` expected


  describe "ignoredPatterns" $ do
    it "loads patterns to ignore from GIT ignore file with default path" $ do
      result <- ignoredPatterns Nothing $ repo </> "a" </> ".gitignore"
      let expected = (repo </> "a" </> ".gitignore", ["*.xml"])
      result `shouldBe` expected

    it "loads patterns to ignore from GIT ignore file with custom path" $ do
      result <- ignoredPatterns (Just "/") $ repo </> "a" </> ".gitignore"
      let expected = ("/", ["*.xml"])
      result `shouldBe` expected
