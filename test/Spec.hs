{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- Spec
-}

import Test.Hspec
import XMLSpec.ParserXMLSpec
import XMLSpec.ParserXMLUtilsSpec
import XMLSpec.ParserXMLElementsSpec
import XMLSpec.ParserXMLHeaderSpec

main :: IO ()
main = hspec $ do
    describe "Test" $ do
        it "should be true" $ do
            True `shouldBe` True
    describe "ParserXMLSpec" $ do
        parserXMLSpec
    describe "ParserXMLUtilsSpec" $ do
        parserXMLUtilsSpec
    describe "ParserXMLElementsSpec" $ do
        parserXMLElementsSpec
    describe "ParserXMLHeaderSpec" $ do
        parserXMLHeaderSpec
        
