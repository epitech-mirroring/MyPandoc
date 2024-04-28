{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- ParserXMLUtilsSpec
-}

module XMLSpec.ParserXMLUtilsSpec (
    parserXMLUtilsSpec
) where

import XML.ParserXMLUtils
import Test.Hspec
import ParserData
import DataStruct

parserXMLUtilsSpec :: Spec
parserXMLUtilsSpec = do
  parseXMLFlagSpec
  parseXMLFlagWithAttrSpec
  isWhiteSpaceSpec
  filterWhiteSpaceSpec

parseXMLFlagSpec :: Spec
parseXMLFlagSpec = do
    describe "parseXMLFlag" $ do
        it "should return the flag" $ do
            runParser (parseXMLFlag "flag") "<flag>" `shouldBe` Just ("flag", "")
        it "should return the flag" $ do
            runParser (parseXMLFlag "flag") "<flag> a" `shouldBe` Just ("flag", " a")
        it "should not return the flag" $ do
            runParser (parseXMLFlag "flag") "a<flag>" `shouldBe` Nothing
        it "should not return the flag" $ do
            runParser (parseXMLFlag "flag") "<flag" `shouldBe` Nothing
        it "should not return the flag" $ do
            runParser (parseXMLFlag "flag") "flag>" `shouldBe` Nothing

parseXMLFlagWithAttrSpec :: Spec
parseXMLFlagWithAttrSpec = do
    describe "parseXMLFlagWithAttr" $ do
        it "should return the flag and the attribute" $ do
            runParser (parseXMLFlagWithAttr "flag" "key") "<flag key=\"attr\">" `shouldBe` Just (("flag", "attr"), "")
        it "should return the flag and the attribute" $ do
            runParser (parseXMLFlagWithAttr "flag" "key") "<flag key=\"attr\"> a" `shouldBe` Just (("flag", "attr"), " a")
        it "should not return the flag and the attribute" $ do
            runParser (parseXMLFlagWithAttr "flag" "key") "a<flag key=\"attr\">" `shouldBe` Nothing
        it "should not return the flag and the attribute" $ do
            runParser (parseXMLFlagWithAttr "flag" "key") "<flag key=\"attr" `shouldBe` Nothing
        it "should not return the flag and the attribute" $ do
            runParser (parseXMLFlagWithAttr "flag" "key") "<flagkey=\"attr\">" `shouldBe` Nothing
        it "should not return the flag and the attribute" $ do
            runParser (parseXMLFlagWithAttr "flag" "key") "<flag key =\"attr\"> a" `shouldBe` Nothing

isWhiteSpaceSpec :: Spec
isWhiteSpaceSpec = do
    describe "isWhiteSpace" $ do
        it "should return True" $ do
            isWhiteSpace (Text " \n\t") `shouldBe` True
        it "should return False" $ do
            isWhiteSpace (Text "a") `shouldBe` False
        it "should return False" $ do
            isWhiteSpace (Bold (Text "a")) `shouldBe` False
        it "should return False" $ do
            isWhiteSpace (Bold (Text "  ")) `shouldBe` False

filterWhiteSpaceSpec :: Spec
filterWhiteSpaceSpec = do
    describe "filterWhiteSpace" $ do
        it "should return an empty list" $ do
            filterWhiteSpace [Text " \n\t"] `shouldBe` []
        it "should return a list with one element" $ do
            filterWhiteSpace [Text "a"] `shouldBe` [Text "a"]
        it "should return a list with one element" $ do
            filterWhiteSpace [Bold (Text "a")] `shouldBe` [Bold (Text "a")]
        it "should return a list with one element" $ do
            filterWhiteSpace [Bold (Text "  ")] `shouldBe` [Bold (Text "  ")]
        it "should return a list with one element" $ do
            filterWhiteSpace [Text "a", Text " \n\t", Bold (Text "a"), Bold (Text " \n\t")] `shouldBe` [Text "a", Bold (Text "a"), Bold (Text " \n\t")]
        it "should return a list with four element" $ do
            filterWhiteSpace [Text "a", Text "a", Bold (Text "a"), Bold (Text "a")] `shouldBe` [Text "a", Text "a", Bold (Text "a"), Bold (Text "a")]
        it "should return a list with one element" $ do
            filterWhiteSpace [Text "a", Text "a", Bold (Text "a"), Bold (Text "a"), Text " \n\t", Text " \n\t"] `shouldBe` [Text "a", Text "a", Bold (Text "a"), Bold (Text "a")]
