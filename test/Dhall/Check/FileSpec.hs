
module Dhall.Check.FileSpec (spec) where

import Dhall.Check.File

import System.Directory (makeAbsolute)
import System.IO.Temp (withTempDirectory, withTempFile)
import Test.Hspec

data TempExample = TempExample [FilePath] [FilePath]

spec :: Spec
spec =
  describe "Test allFiles function" $ do
    (md, hs, dhall, files) <- runIO testAllFiles
    it "should return the Markdown file" $ do
      checkTempExample md

    it "should return all Haskell Files" $ do
      checkTempExample hs

    it "should return all Dhall Files" $ do
      checkTempExample dhall

    it "should return all Files" $ do
      checkTempExample files

  where
    checkTempExample (TempExample e r) = e `shouldBe` r

    testAllFiles :: IO (TempExample, TempExample, TempExample, TempExample)
    testAllFiles =
      withTempDirectory "." "goldenfiles" $ \dir ->
        withTempFile dir "Test.md" $ \mdTemp _ ->
        withTempFile dir "Test.hs" $ \hsTemp _ ->
        withTempFile dir "Test.dhall" $ \dhallTemp _ -> do
          md <- allFiles dir [".md"]
          mdExpected <- makeAbsolute mdTemp

          hs <- allFiles dir [".hs"]
          hsExpected <- makeAbsolute hsTemp

          dhall <- allFiles dir [".dhall"]
          dhallExpected <- makeAbsolute dhallTemp

          files <- allFiles dir [".md", ".hs", ".dhall"]

          pure ( TempExample [mdExpected] md
               , TempExample [hsExpected] hs
               , TempExample [dhallExpected] dhall
               , TempExample [mdExpected, hsExpected, dhallExpected] files
               )
