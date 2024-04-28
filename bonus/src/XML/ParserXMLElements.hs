{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- ParserXMLElements
-}

module XML.ParserXMLElements (
        parseXMLText,
        parseXMLBold,
        parseXMLItalic,
        parseXMLCode,
        parseXMLList,
        parseXMLParagraph,
        parseXMLCodeBlock,
        parseXMLLink,
        parseXMLImage,
        parseXMLSection,
        parseXMLElement
    ) where

import ParserData (
        Parser(..),
        parseSome,
        parseMany,
        parseNotChar,
        parseWhiteSpace
    )

import DataStruct (
        Element(..),
        LinkType(..),
        ImageType(..),
        SectionType(..)
    )

import XML.ParserXMLUtils (
        parseXMLFlag,
        parseXMLFlagWithAttr,
        filterWhiteSpace
    )
import Control.Applicative (Alternative(..))

parseXMLText :: Parser Element
parseXMLText = do
    text <- parseSome (parseNotChar '<')
    return (Text text)

parseXMLBold :: Parser Element
parseXMLBold = do
    _ <- parseWhiteSpace
    _ <- parseXMLFlag "bold"
    element <- parseXMLElement
    _ <- parseXMLFlag "/bold"
    return (Bold element)

parseXMLItalic :: Parser Element
parseXMLItalic = do
    _ <- parseWhiteSpace
    _ <- parseXMLFlag "italic"
    element <- parseXMLElement
    _ <- parseXMLFlag "/italic"
    return (Italic element)

parseXMLCode :: Parser Element
parseXMLCode = do
    _ <- parseWhiteSpace
    _ <- parseXMLFlag "code"
    element <- parseXMLElement
    _ <- parseXMLFlag "/code"
    return (Code element)

parseXMLList :: Parser Element
parseXMLList = do
    _ <- parseWhiteSpace
    _ <- parseXMLFlag "list"
    elems <- parseMany parseXMLElement
    _ <- parseXMLFlag "/list"
    _ <- parseWhiteSpace
    return (List (filterWhiteSpace elems))

parseXMLParagraph :: Parser Element
parseXMLParagraph = do
    _ <- parseWhiteSpace
    _ <- parseXMLFlag "paragraph"
    elems <- parseMany parseXMLElement
    _ <- parseXMLFlag "/paragraph"
    _ <- parseWhiteSpace
    return (Paragraph elems)

parseXMLCodeBlock :: Parser Element
parseXMLCodeBlock = do
    _ <- parseWhiteSpace
    _ <- parseXMLFlag "codeblock"
    elems <- parseMany parseXMLElement
    _ <- parseXMLFlag "/codeblock"
    _ <- parseWhiteSpace
    return (CodeBlock (filterWhiteSpace elems))

parseXMLLink :: Parser Element
parseXMLLink = do
    _ <- parseWhiteSpace
    (_, urlContent) <- parseXMLFlagWithAttr "link" "url"
    elems <- parseMany parseXMLElement
    _ <- parseXMLFlag "/link"
    _ <- parseWhiteSpace
    return (Link (LinkType urlContent elems))

parseXMLImage :: Parser Element
parseXMLImage = do
    _ <- parseWhiteSpace
    (_, urlContent) <- parseXMLFlagWithAttr "image" "url"
    altContent <- parseMany parseXMLElement
    _ <- parseXMLFlag "/image"
    _ <- parseWhiteSpace
    return (Image (ImageType urlContent altContent))

parseXMLSection :: Parser Element
parseXMLSection = do
    _ <- parseWhiteSpace
    (_, title) <- parseXMLFlagWithAttr "section" "title"
    elems <- parseMany parseXMLElement
    _ <- parseXMLFlag "/section"
    _ <- parseWhiteSpace
    return (Section (SectionType title (filterWhiteSpace elems)))

parseXMLElement :: Parser Element
parseXMLElement = do
    element <- parseXMLBold <|> parseXMLItalic <|> parseXMLCode
        <|> parseXMLList <|> parseXMLParagraph <|> parseXMLCodeBlock
        <|> parseXMLLink <|> parseXMLImage <|> parseXMLSection <|> parseXMLText
    return element
