{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- ParserJSONElements
-}

module JSON.ParserJSONElements (
        parseJSONText,
        parseJSONBold,
        parseJSONItalic,
        parseJSONCode,
        parseJSONList,
        parseJSONParagraph,
        parseJSONCodeBlock,
        parseJSONLink,
        parseJSONImage,
        parseJSONSection,
        parseJSONElement,
        parseJSONElements
    ) where

import DataStruct (
        Element(..),
        LinkType(..),
        ImageType(..),
        SectionType(..)
    )

import ParserData (
        Parser(..),
        parseChar,
        parseString,
        parseSome,
        parseNotChar,
        parseEmptyString,
        parseEmptyList,
        parseEmpty
    )

import Control.Applicative (Alternative(..))

parseJSONText :: Parser Element
parseJSONText = do
    _ <- parseChar '\"'
    text <- parseSome (parseNotChar '\"') <|> parseEmptyString '*'
    _ <- parseChar '\"'
    return (Text text)

parseJSONBold :: Parser Element
parseJSONBold = do
    _ <- parseChar '{'
    _ <- parseString "\"bold\":"
    element <- parseJSONElement <|> parseEmpty '*'
    _ <- parseChar '}'
    return (Bold element)

parseJSONItalic :: Parser Element
parseJSONItalic = do
    _ <- parseChar '{'
    _ <- parseString "\"italic\":"
    element <- parseJSONElement <|> parseEmpty '*'
    _ <- parseChar '}'
    return (Italic element)

parseJSONCode :: Parser Element
parseJSONCode = do
    _ <- parseChar '{'
    _ <- parseString "\"code\":"
    element <- parseJSONElement <|> parseEmpty '*'
    _ <- parseChar '}'
    return (Code element)

parseJSONList :: Parser Element
parseJSONList = do
    _ <- parseChar '{'
    _ <- parseString "\"list\":"
    _ <- parseChar '['
    elements <- parseJSONElements ']' <|> parseEmptyList ']'
    _ <- parseChar '}'
    return (List elements)

parseJSONParagraph :: Parser Element
parseJSONParagraph = do
    _ <- parseChar '['
    elements <- parseJSONElements ']' <|> parseEmptyList ']'
    return (Paragraph elements)

parseJSONCodeBlock :: Parser Element
parseJSONCodeBlock = do
    _ <- parseChar '{'
    _ <- parseString "\"codeblock\":"
    _ <- parseChar '['
    elements <- parseJSONElements ']' <|> parseEmptyList ']'
    _ <- parseChar '}'
    return (CodeBlock elements)

parseJSONLink :: Parser Element
parseJSONLink = do
    _ <- parseChar '{'
    _ <- parseString "\"link\":{\"url\":"
    _ <- parseChar '\"'
    strUrl <- parseSome (parseNotChar '\"') <|> parseEmptyString '*'
    _ <- parseString "\",\"content\":["
    elements <- parseJSONElements ']' <|> parseEmptyList ']'
    _ <- parseChar '}'
    _ <- parseChar '}'
    return (Link (LinkType strUrl elements))

parseJSONImage :: Parser Element
parseJSONImage = do
    _ <- parseChar '{'
    _ <- parseString "\"image\":{\"url\":\""
    strUrl <- parseSome (parseNotChar '\"') <|> parseEmptyString '*'
    _ <- parseString "\",\"alt\":["
    elements <- parseJSONElements ']' <|> parseEmptyList ']'
    _ <- parseChar '}'
    _ <- parseChar '}'
    return (Image (ImageType strUrl elements))

parseJSONSection :: Parser Element
parseJSONSection = do
    _ <- parseString "{\"section\":{\"title\":"
    _ <- parseChar '\"'
    title <- parseSome (parseNotChar '\"') <|> parseEmptyString '*'
    _ <- parseString "\",\"content\":["
    elements <- parseJSONElements ']' <|> parseEmptyList ']'
    _ <- parseChar '}'
    _ <- parseChar '}'
    return (Section (SectionType title elements))

parseJSONElement :: Parser Element
parseJSONElement = do
    element <- parseJSONText <|> parseJSONBold <|> parseJSONItalic
        <|> parseJSONCode <|> parseJSONList <|> parseJSONParagraph
        <|> parseJSONCodeBlock <|> parseJSONLink <|> parseJSONImage
        <|> parseJSONSection
    return element

parseJSONElements :: Char -> Parser [Element]
parseJSONElements c = do
    element <- parseJSONElement
    x <- parseChar ',' <|> parseChar c
    case x of
        ',' -> do
            elements <- parseJSONElements c
            return (element : elements)
        _ -> return [element]
