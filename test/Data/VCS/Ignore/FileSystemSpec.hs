module Data.VCS.Ignore.FileSystemSpec
  ( spec
  )
where

import qualified Data.List                     as L
import           Data.VCS.Ignore.FileSystem
import           Test.Hspec


spec :: Spec
spec = do
  describe "findFiles" $ do
    it "recursively finds files filtered by given predicate" $ do
      let expected  = ["test-data/list-files/dir1/dir2/d.xml"]
          predicate = ("d.xml" `L.isSuffixOf`)
      files <- findFiles "test-data/list-files/" predicate
      files `shouldBe` expected


  describe "listFiles" $ do
    it "returns empty list if path doesn't exist" $ do
      filePaths <- listFiles "non-existing-path"
      filePaths `shouldBe` []

    it "recursively finds all files in directory" $ do
      filePaths <- listFiles "test-data/list-files/"
      let expected =
            [ "test-data/list-files/a.txt"
            , "test-data/list-files/dir1/b.txt"
            , "test-data/list-files/dir1/dir2/c.txt"
            , "test-data/list-files/dir1/dir2/d.xml"
            ]
      L.sort filePaths `shouldBe` L.sort expected
