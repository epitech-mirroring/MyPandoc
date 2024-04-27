{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- ParserXMLSpec
-}

module XMLSpec.ParserXMLSpec (
    parserXMLSpec
) where

import Test.Hspec
import XML.ParserXML
import DataStruct
import ParserData

parserXMLSpec :: Spec
parserXMLSpec = do
    parseXMLBodySpec
    parseXMLDocumentSpec
    detectXMLDocumentSpec

parseXMLBodySpec :: Spec
parseXMLBodySpec = describe "parseXMLBody" $ do
    it "should parse a body" $ do
        runParser parseXMLBody "<body><bold>text</bold></body>" `shouldBe` Just ((Body [Bold (Text "text")], ""))
    it "should not parse a body" $ do
        runParser parseXMLBody "<body><bold>text</bold>" `shouldBe` Nothing
    it "should not parse a body" $ do
        runParser parseXMLBody "<body><bold>text" `shouldBe` Nothing
    it "should not parse a body" $ do
        runParser parseXMLBody "<body><bold>text</bold></body>a" `shouldBe` Just ((Body [Bold (Text "text")], "a"))
    it "should not parse a body" $ do
        runParser parseXMLBody "a<body><bold>text</bold></body>" `shouldBe` Nothing
    it "should not parse a body" $ do
        runParser parseXMLBody "<bady><bold>text</bold></bady>" `shouldBe` Nothing

parseXMLDocumentSpec :: Spec
parseXMLDocumentSpec = describe "parseXMLDocument" $ do
    it "should parse a document" $ do
        runParser parseXMLDocument "<document><header title=\"title\"></header><body><bold>text</bold></body></document>" `shouldBe` Just ((Document (Header [Title "title"]) (Body [Bold (Text "text")]), ""))
    it "should parse a document" $ do
        runParser parseXMLDocument "<document><header title=\"title\"></header><body><bold>text</bold></body></document> a" `shouldBe` Just ((Document (Header [Title "title"]) (Body [Bold (Text "text")]), "a"))
    it "should not parse a document" $ do
        runParser parseXMLDocument "a<document><header title=\"title\"></header><body><bold>text</bold></body></document>" `shouldBe` Nothing
    it "should not parse a document" $ do
        runParser parseXMLDocument "<document><header title=\"title\"></header><body><bold>text</bold></body>" `shouldBe` Nothing
    it "should not parse a document" $ do
        runParser parseXMLDocument "a<document><header title=\"title\"></header><body><bold>text</bold></body>" `shouldBe` Nothing
    it "should not parse a document" $ do
        runParser parseXMLDocument "<document>a<header title=\"title\"></header><body><bold>text</bold></body></document>" `shouldBe` Nothing
    it "should not parse a document" $ do
        runParser parseXMLDocument "<document><header title=\"title\"></header><body><bold>text</bold></body>a</document>" `shouldBe` Nothing
    it "should parse a document" $ do
        runParser parseXMLDocument "<document> \n  \n\t<header title=\"title\"></header> \n \n \t \t <body><bold>text</bold></body>\t \t\n  </document>" `shouldBe` Just ((Document (Header [Title "title"]) (Body [Bold (Text "text")]), ""))
    it "should not parse a document" $ do
        runParser parseXMLDocument "<document> \n  \n\t<header title=\"title\"></header> \n \n \t \t <body><bold>text</bold></body>\t \t\n" `shouldBe` Nothing

detectXMLDocumentSpec :: Spec
detectXMLDocumentSpec = describe "detectXMLDocument" $ do
    it "should get a document" $ do
        getXMLDocument "<document><header title=\"title\"></header><body><bold>text</bold></body></document>" `shouldBe` Just (Document (Header [Title "title"]) (Body [Bold (Text "text")]))
    it "should not get a document" $ do
        getXMLDocument "<document><header title=\"title\"></header><body><bold>text</bold></body></document>aa" `shouldBe` Nothing
