{-
-- EPITECH PROJECT, 2024
-- My-Image-Compressor
-- File description:
-- AlgorithmSpecs
-}

module HandleArgsSpecs (
    handleArgsSpecs
) where

import Test.Hspec ( describe, it, shouldBe, Spec )
import HandleArgs (getHelp, checkFormat, options, Options(..))

handleArgsSpecs :: Spec
handleArgsSpecs = describe "Handle args test:" $ do
    getHelpSpecs

getHelpSpecs :: Spec
getHelpSpecs = describe "getHelp" $ do
    it "should return the help message" $ do
        getHelp `shouldBe`
            "USAGE: ./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]\
            \-i ifile    --ifile=ifile      path to the file to convert\
            \-o ofile    --ofile=ofile      path to the output file\
            \-f oformat  --oformat=oformat  output format (xml, json, markdown)\
            \-e iformat  --iformat=iformat  input format (xml, json, markdown)"

checkFormatSpecs :: Spec
checkFormatSpecs = describe "checkFormat" $ do
    it "should return the format" $ do
        checkFormat "xml" `shouldBe` Just "xml"
        checkFormat "json" `shouldBe` Just "json"
        checkFormat "markdown" `shouldBe` Just "markdown"
        checkFormat "txt" `shouldBe` Nothing
        checkFormat "pdf" `shouldBe` Nothing
        checkFormat "" `shouldBe` Nothing
