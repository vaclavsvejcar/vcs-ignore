{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE StrictData       #-}
{-# LANGUAGE TypeApplications #-}

module Data.VCS.Ignore.CoreSpec
  ( spec
  )
where

import qualified Data.List                     as L
import           Data.Maybe                     ( catMaybes )
import           Data.VCS.Ignore.Core
import           Data.VCS.Ignore.Repo           ( Repo(..) )
import           System.FilePath                ( (</>) )
import           Test.Hspec


spec :: Spec
spec = do
  let testRepoPath = "test-data" </> "test-repo"

  describe "listRepo" $ do
    it "lists repository paths, based on the search filter" $ do
      let pathFilter = mempty
          expected =
            [ testRepoPath
            , testRepoPath </> "file1.txt"
            , testRepoPath </> "file2.txt"
            ]
      repo   <- scanRepo @TestRepo testRepoPath
      result <- listRepo repo pathFilter
      L.sort result `shouldBe` L.sort expected


  describe "walkRepo" $ do
    it "walks repository paths, based on the search filter" $ do
      pending


data TestRepo = TestRepo
  { trPath :: FilePath
  }
  deriving (Eq, Show)

instance Repo TestRepo where
  repoPath TestRepo {..} = trPath

  scanRepo path = pure TestRepo { trPath = path }

  isExcluded _ path = "excluded.txt" `L.isSuffixOf` path
