{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- ParserXML
-}

module XML.ParserXML (
        parseXMLBody,
        parseXMLDocument,
        getXMLDocument
    ) where

import ParserData (
        Parser(..),
        runParser,
        parseWhiteSpace,
        parseMany
    )

import XML.ParserXMLUtils (parseXMLFlag)
import XML.ParserXMLElements (parseXMLElement)
import XML.ParserXMLHeader (parseXMLHeader)
import DataStruct (Document(..), Body(..))

parseXMLBody :: Parser Body
parseXMLBody = do
    _ <- parseXMLFlag "body"
    elems <- parseMany parseXMLElement
    _ <- parseXMLFlag "/body"
    return (Body elems)

parseXMLDocument :: Parser Document
parseXMLDocument = do
    _ <- parseXMLFlag "document"
    _ <- parseWhiteSpace
    headerContent <- parseXMLHeader
    _ <- parseWhiteSpace
    bodyContent <- parseXMLBody
    _ <- parseWhiteSpace
    _ <- parseXMLFlag "/document"
    _ <- parseWhiteSpace
    return (Document headerContent bodyContent)

getXMLDocument :: String -> Maybe Document
getXMLDocument s = case runParser parseXMLDocument s of
    Just (doc, "") -> Just doc
    _ -> Nothing
