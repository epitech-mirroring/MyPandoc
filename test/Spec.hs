{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- Spec
-}

import Test.Hspec
import HandleArgsSpecs (handleArgsSpecs)
import OpenFileSpecs (openFilesSpecs)
import XMLSpec.ParserXMLSpec (parserXMLSpec)
import XMLSpec.ParserXMLUtilsSpec (parserXMLUtilsSpec)
import XMLSpec.ParserXMLElementsSpec (parserXMLElementsSpec)
import XMLSpec.ParserXMLHeaderSpec (parserXMLHeaderSpec)
import DataStructSpecs (dataStructSpecs)
import ParserDataSpecs (parserDataSpecs)

main :: IO ()
main = hspec $ do
    describe "ParserXMLSpec" $ do
        parserXMLSpec
    describe "ParserXMLUtilsSpec" $ do
        parserXMLUtilsSpec
    describe "ParserXMLElementsSpec" $ do
        parserXMLElementsSpec
    describe "ParserXMLHeaderSpec" $ do
        parserXMLHeaderSpec
    describe "ArgumentHandlerSpec" $ do
        handleArgsSpecs
    describe "OpenFileSpec" $ do 
        openFilesSpecs
    dataStructSpecs
    parserDataSpecs
