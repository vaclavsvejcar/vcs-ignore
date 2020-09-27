{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE StrictData       #-}
{-# LANGUAGE TypeApplications #-}

module Data.VCS.Ignore.CoreSpec
  ( spec
  )
where

import qualified Data.List                     as L
import           Data.VCS.Ignore.Core
import           Data.VCS.Ignore.PathFilter     ( PathFilter(..)
                                                , notMatched
                                                )
import           Data.VCS.Ignore.Repo           ( Repo(..) )
import           System.FilePath                ( (</>) )
import           Test.Hspec


spec :: Spec
spec = do
  let testRepoRoot = "test-data" </> "test-repo"

  describe "listRepo" $ do
    it "lists repository paths, based on the search filter" $ do
      let pathFilter = mempty
          expected =
            [ testRepoRoot
            , testRepoRoot </> "file1.txt"
            , testRepoRoot </> "file2.txt"
            ]
      repo   <- scanRepo @TestRepo testRepoRoot
      result <- listRepo repo pathFilter
      L.sort result `shouldBe` L.sort expected


  describe "walkRepo" $ do
    it "walks repository paths, based on the search filter" $ do
      let pathFilter = excludeFile2
          expected   = [testRepoRoot, testRepoRoot </> "file1.txt"]
      repo   <- scanRepo @TestRepo testRepoRoot
      result <- listRepo repo pathFilter
      L.sort result `shouldBe` L.sort expected


data TestRepo = TestRepo
  { trPath :: FilePath
  }
  deriving (Eq, Show)

instance Repo TestRepo where
  repoRoot TestRepo {..} = trPath

  scanRepo path = pure TestRepo { trPath = path }

  isExcluded _ path = "excluded.txt" `L.isSuffixOf` path


excludeFile2 :: PathFilter
excludeFile2 = PathFilter $ \case
  path | "file2.txt" `L.isSuffixOf` path -> notMatched path
  path -> pure path


