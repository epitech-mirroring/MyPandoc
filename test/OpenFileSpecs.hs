{-
-- EPITECH PROJECT, 2024
-- My Pandoc
-- File description:
-- spec for OpenFile
-}

module OpenFileSpecs (
    openFilesSpecs
) where

import Test.Hspec
import OpenFile (wichFormat, getFormat, getOutput, openFile)
import HandleArgs

openFilesSpecs :: Spec
openFilesSpecs = describe "openFile" $ do
    wichFormatSpecs
    getFormatSpecs
    getOutputSpecs

wichFormatSpecs :: Spec
wichFormatSpecs = describe "wichFormat" $ do
    it "should return the format of the file xml" $ do
        result <- readFile "./test/test_files/syntaxe.xml"
        wichFormat result `shouldBe` "xml"
    it "should return the format of the file json" $ do
        result <- readFile "./test/test_files/syntaxe.json"
        wichFormat result `shouldBe` "json"
    it "should return the format of the file markdown" $ do
        result <- readFile "./test/test_files/syntaxe.md"
        wichFormat result `shouldBe` "markdown"
    it "should return the format of the file empty" $ do
        wichFormat "" `shouldBe` ""
    it "should return the format of the file empty" $ do
        wichFormat "test" `shouldBe` ""

getFormatSpecs :: Spec
getFormatSpecs = describe "getFormat" $ do
    it "should return the format of the file" $ do
        getFormat (Just "xml") "test" `shouldBe` "xml"
    it "should return the format of the file" $ do
        getFormat Nothing "test" `shouldBe` ""

getOutputSpecs :: Spec
getOutputSpecs = describe "getOutput" $ do
    it "should return the output of the file" $ do
        getOutput (Just "test") `shouldBe` "test"
    it "should return the output of the file" $ do
        getOutput Nothing `shouldBe` "stdout"

