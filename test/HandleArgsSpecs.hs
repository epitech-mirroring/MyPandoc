{-
-- EPITECH PROJECT, 2024
-- My-Image-Compressor
-- File description:
-- Specs for the HandleArgs module
-}

module HandleArgsSpecs (
    handleArgsSpecs
) where

import Test.Hspec ( describe, it, shouldBe, Spec, shouldReturn, shouldEndWith, shouldThrow, anyException )
import HandleArgs (getHelp, checkFormat, options, Options(..), parseOptions, parseArgs, checkArgs)
import System.Console.GetOpt (getOpt, ArgOrder(..))


handleArgsSpecs :: Spec
handleArgsSpecs = describe "Handle args test:" $ do
    getHelpSpecs
    checkFormatSpecs
    parseOptionsSpecs
    checkArgsSpecs
    optionsSpecs

getHelpSpecs :: Spec
getHelpSpecs = describe "----getHelp" $ do
    it "should return the help message" $ do
        getHelp `shouldBe`
            "USAGE: ./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]\n\
            \  -i ifile    --ifile=ifile      path to the file to convert\n\
            \  -o ofile    --ofile=ofile      path to the output file\n\
            \  -f oformat  --oformat=oformat  output format (xml, json, markdown)\n\
            \  -e iformat  --iformat=iformat  input format (xml, json, markdown)\n"


optionsSpecs :: Spec
optionsSpecs = describe "----Options" $ do
    it "should return the options" $ do
        Options {
            oIformat = Just "xml",
            oOformat = Just "xml",
            oOutput = Just "ofile",
            oInput = Just "ifile"
        } `shouldBe` Options {
            oIformat = Just "xml",
            oOformat = Just "xml",
            oOutput = Just "ofile",
            oInput = Just "ifile"
        }
    it "should return the options" $ do
        Options {
            oIformat = Nothing,
            oOformat = Just "xml",
            oOutput = Just "ofile",
            oInput = Just "ifile"
        } `shouldBe` Options {
            oIformat = Nothing,
            oOformat = Just "xml",
            oOutput = Just "ofile",
            oInput = Just "ifile"
        }

checkFormatSpecs :: Spec
checkFormatSpecs = describe "----checkFormat" $ do
    it "should return the format" $ do
        checkFormat "xml" `shouldBe` Just "xml"
    it "should return the format" $ do
        checkFormat "json" `shouldBe` Just "json"
    it "should return the format" $ do
        checkFormat "markdown" `shouldBe` Just "markdown"
    it "should return Nothing" $ do
        checkFormat "txt" `shouldBe` Nothing
    it "should return Nothing" $ do
        checkFormat "pdf" `shouldBe` Nothing
    it "should return Nothing" $ do
        checkFormat "" `shouldBe` Nothing

parseOptionsSpecs :: Spec
parseOptionsSpecs = describe "----parseOptions" $ do
    it "should return the options 1" $ do
        parseOptions ["-i", "ifile", "-f", "xml"] `shouldReturn` Options {
            oIformat = Nothing,
            oOformat = Just "xml",
            oOutput = Nothing,
            oInput = Just "ifile"
        }
    it "should return the options 2" $ do
        parseOptions ["-i", "ifile", "-f", "xml", "-o", "ofile"] `shouldReturn` Options {
            oIformat = Nothing,
            oOformat = Just "xml",
            oOutput = Just "ofile",
            oInput = Just "ifile"
        }
    it "should return the options 3" $ do
        parseOptions ["-i", "ifile", "-f", "xml", "-o", "ofile", "-e", "xml"] `shouldReturn` Options {
            oIformat = Just "xml",
            oOformat = Just "xml",
            oOutput = Just "ofile",
            oInput = Just "ifile"
        }
    it "should return the options 4" $ do
        parseOptions ["-f", "xml", "-o", "ofile", "-e", "json"] `shouldReturn` Options {
            oIformat = Just "json",
            oOformat = Just "xml",
            oOutput = Just "ofile",
            oInput = Nothing
        }
    it "should return the options 5" $ do
        parseOptions [] `shouldReturn` Options {
            oIformat = Nothing,
            oOformat = Nothing,
            oOutput = Nothing,
            oInput = Nothing
        }
    it "should exit" $ do
        parseOptions ["-i", "-f", "txt"] `shouldThrow` anyException

checkArgsSpecs :: Spec
checkArgsSpecs = describe "----checkArgs" $ do
    it "Missing all mandatory" $ do
        checkArgs Options {
            oIformat = Nothing,
            oOformat = Nothing,
            oOutput = Nothing,
            oInput = Nothing
        } `shouldBe` Nothing
    it "Missing output format file" $ do
        checkArgs Options {
            oIformat = Just "xml",
            oOformat = Nothing,
            oOutput = Nothing,
            oInput = Nothing
        } `shouldBe` Nothing
    it "Missing input file" $ do
        checkArgs Options {
            oIformat = Just "xml",
            oOformat = Just "xml",
            oOutput = Just "ofile",
            oInput = Nothing
        } `shouldBe` Nothing
    it "Missing input format" $ do
        checkArgs Options {
            oIformat = Nothing,
            oOformat = Just "xml",
            oOutput = Just "ofile",
            oInput = Just "ifile"
        } `shouldBe` Just Options {
            oIformat = Nothing,
            oOformat = Just "xml",
            oOutput = Just "ofile",
            oInput = Just "ifile"
        }
    it "all good" $ do
        checkArgs Options {
            oIformat = Just "xml",
            oOformat = Just "xml",
            oOutput = Just "ofile",
            oInput = Just "ifile"
        } `shouldBe` Just Options {
            oIformat = Just "xml",
            oOformat = Just "xml",
            oOutput = Just "ofile",
            oInput = Just "ifile"
        }
    it "Invalid input format" $ do
        checkArgs Options {
            oIformat = Just "txt",
            oOformat = Just "xml",
            oOutput = Just "ofile",
            oInput = Just "ifile"
        } `shouldBe` Nothing
