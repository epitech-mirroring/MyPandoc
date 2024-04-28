{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- ParserXMLHeaderSpec
-}

module XMLSpec.ParserXMLHeaderSpec (
    parserXMLHeaderSpec
) where

import XML.ParserXMLHeader
import Test.Hspec
import ParserData
import DataStruct

parserXMLHeaderSpec :: Spec
parserXMLHeaderSpec = do
  parseXMLHeaderAuthorAndDateSpec
  parseXMLHeaderSpec

parseXMLHeaderAuthorAndDateSpec :: Spec
parseXMLHeaderAuthorAndDateSpec = describe "parseXMLHeaderAuthorAndDate" $ do
    it "should parse a header with author" $ do
        runParser (parseXMLHeaderAuthorAndDate "title" "author") "author</author></header>" `shouldBe` Just (Header [Title "title", Author "author"], "")
    it "should parse a header with author" $ do
        runParser (parseXMLHeaderAuthorAndDate "title" "author") "author</author></header> a" `shouldBe` Just (Header [Title "title", Author "author"], " a")
    it "should parse a header with author and date" $ do
        runParser (parseXMLHeaderAuthorAndDate "title" "author") "author</author><date>date</date></header>" `shouldBe` Just (Header [Title "title", Author "author", Date "date"], "")
    it "should parse a header with author and date" $ do
        runParser (parseXMLHeaderAuthorAndDate "title" "author") "author</author><date>date</date></header> a" `shouldBe` Just (Header [Title "title", Author "author", Date "date"], " a")
    it "should parse a header with date" $ do
        runParser (parseXMLHeaderAuthorAndDate "title" "date") "date</date></header>" `shouldBe` Just (Header [Title "title", Date "date"], "")
    it "should parse a header with date" $ do
        runParser (parseXMLHeaderAuthorAndDate "title" "date") "date</date></header> a" `shouldBe` Just (Header [Title "title", Date "date"], " a")
    it "should parse a header with date and author" $ do
        runParser (parseXMLHeaderAuthorAndDate "title" "date") "date</date><author>author</author></header>" `shouldBe` Just (Header [Title "title", Author "author", Date "date"], "")
    it "should not parse a header" $ do
        runParser (parseXMLHeaderAuthorAndDate "title" "author") "a<author>ab</header>" `shouldBe` Nothing
    it "should be an error" $ do
        runParser (parseXMLHeaderAuthorAndDate "title" "error") "author</author" `shouldBe` Nothing

parseXMLHeaderSpec :: Spec
parseXMLHeaderSpec = describe "parseXMLHeader" $ do
    it "should parse a header" $ do
        runParser parseXMLHeader "<header title=\"title\"></header>" `shouldBe` Just (Header [Title "title"], "")
    it "should parse a header" $ do
        runParser parseXMLHeader "<header title=\"title\"></header> a" `shouldBe` Just (Header [Title "title"], " a")
    it "should parse a header with author" $ do
        runParser parseXMLHeader "<header title=\"title\"><author>author</author></header>" `shouldBe` Just (Header [Title "title", Author "author"], "")
    it "should parse a header with author" $ do
        runParser parseXMLHeader "<header title=\"title\"><author>author</author></header> a" `shouldBe` Just (Header [Title "title", Author "author"], " a")
    it "should parse a header with date" $ do
        runParser parseXMLHeader "<header title=\"title\"><date>date</date></header>" `shouldBe` Just (Header [Title "title", Date "date"], "")
    it "should parse a header with date" $ do
        runParser parseXMLHeader "<header title=\"title\"><date>date</date></header> a" `shouldBe` Just (Header [Title "title", Date "date"], " a")
    it "should parse a header with author and date" $ do
        runParser parseXMLHeader "<header title=\"title\"><author>author</author><date>date</date></header>" `shouldBe` Just (Header [Title "title", Author "author", Date "date"], "")
    it "should parse a header with author and date" $ do
        runParser parseXMLHeader "<header title=\"title\"><author>author</author><date>date</date></header> a" `shouldBe` Just (Header [Title "title", Author "author", Date "date"], " a")
    it "should not parse a header" $ do
        runParser parseXMLHeader "a<header title=\"title\">ab</header>" `shouldBe` Nothing
    it "should not parse a header" $ do
        runParser parseXMLHeader "<header title=\"title\"></header" `shouldBe` Nothing
