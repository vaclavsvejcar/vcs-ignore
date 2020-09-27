module Data.VCS.Ignore.Repo.GitSpec
  ( spec
  )
where


import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Data.Map                      as M
import           Data.VCS.Ignore.Repo.Git
import           System.FilePath                ( (</>) )
import           Test.Hspec


spec :: Spec
spec = do
  let repo = "test-data" </> "fake-git-repo"


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

    it "loads empty pattern list if target file doesn't exist" $ do
      result <- ignoredPatterns Nothing $ repo </> "non-existing"
      let expected = (repo </> "non-existing", [])
      result `shouldBe` expected


  describe "scanRepo'" $ do
    it "searches for all required info in given GIT repo" $ do
      let
        expected = Git
          { gitIgnoredPatterns = M.fromList
                                   [ ("/", [])
                                   , ( "test-data/fake-git-repo/a/.gitignore"
                                     , ["*.xml"]
                                     )
                                   , ( "test-data/fake-git-repo/a/b/.gitignore"
                                     , ["*.txt"]
                                     )
                                   ]
          , gitRepoRoot        = repo
          }
        ne = "non-existent"
      result <- scanRepo' (pure ne) (const ne) dotGitIgnores repo
      result `shouldBe` expected
