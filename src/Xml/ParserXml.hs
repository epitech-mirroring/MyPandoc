{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- ParserXml
-}

module Xml.ParserXml (
        getXmlDocument
    ) where

import ParserData
import DataStruct
import Control.Applicative (Alternative(..))

------------

parseWhiteSpace :: Parser String
parseWhiteSpace = parseMany (parseAnyChar " \n\t")

parseNotChar :: Char -> Parser Char
parseNotChar c = Parser p
    where
        p "" = Nothing
        p (x:xs)
            | c /= x = Just (x, xs)
            | otherwise = Nothing

parseFlag :: String -> Parser String
parseFlag flag =
    parseChar '<' >>
    parseString flag >>
    parseChar '>' >>
    return flag

parseFlagWithAttr :: String -> String -> Parser (String, String)
parseFlagWithAttr flag key = do
    parseChar '<'
    parseString flag
    parseChar ' '
    parseString key
    parseString "=\""
    attr <- parseMany (parseNotChar '"')
    parseString "\">"
    return (flag, attr)

parseHeaderAuthorAndDate :: String -> String -> Parser Header
parseHeaderAuthorAndDate title "author" = do
    author <- parseMany (parseNotChar '<')
    parseAnd (parseFlag "/author") parseWhiteSpace
    arg <- parseFlag "date" <|> parseFlag "/header"
    (if arg == "/header" then return (Header [Title title, Author author])
    else do
        parseWhiteSpace
        date <- parseMany (parseNotChar '<')
        parseFlag "/date" *> parseWhiteSpace *> parseFlag "/header"
        return (Header [Title title, Author author, Date date]))
parseHeaderAuthorAndDate title "date" = do
    date <- parseMany (parseNotChar '<')
    parseAnd (parseFlag "/date") parseWhiteSpace
    arg <- parseFlag "/header" <|> parseFlag "author"
    (if arg == "/header" then return (Header [Title title, Date date])
    else do
        parseWhiteSpace
        author <- parseMany (parseNotChar '<')
        parseFlag "/author" *> parseWhiteSpace *> parseFlag "/header"
        return (Header [Title title, Author author, Date date]))

parseHeader :: Parser Header
parseHeader = do
    parseWhiteSpace
    (_, title) <- parseFlagWithAttr "header" "title"
    parseWhiteSpace
    arg <- parseFlag "/header" <|> parseFlag "author" <|> parseFlag "date"
    if arg == "/header" then
        return (Header [Title title])
    else
      parseWhiteSpace >>
      parseHeaderAuthorAndDate title arg

----- Body -----

parseText :: Parser Element
parseText = do
    text <- parseSome (parseNotChar '<')
    return (Text text)

parseBold :: Parser Element
parseBold = do
    parseWhiteSpace
    parseFlag "bold"
    elem <- parseElement
    parseFlag "/bold"
    return (Bold elem)

parseItalic :: Parser Element
parseItalic = do
    parseWhiteSpace
    parseFlag "italic"
    elem <- parseElement
    parseFlag "/italic"
    return (Italic elem)

parseCode :: Parser Element
parseCode = do
    parseWhiteSpace
    parseFlag "code"
    elem <- parseElement
    parseFlag "/code"
    return (Code elem)

parseList :: Parser Element
parseList = do
    parseWhiteSpace
    parseFlag "list"
    elems <- parseMany parseElement
    parseFlag "/list"
    return (List (filterWhiteSpace elems))

parseParagraph :: Parser Element
parseParagraph = do
    parseWhiteSpace
    parseFlag "paragraph"
    elems <- parseMany parseElement
    parseFlag "/paragraph"
    return (Paragraph elems)

parseCodeBlock :: Parser Element
parseCodeBlock = do
    parseWhiteSpace
    parseFlag "codeblock"
    elems <- parseMany parseElement
    parseFlag "/codeblock"
    return (CodeBlock (filterWhiteSpace elems))

parseLink :: Parser Element
parseLink = do
    parseWhiteSpace
    (_, url) <- parseFlagWithAttr "link" "url"
    elems <- parseMany parseElement
    parseFlag "/link"
    return (Link (LinkType url elems))

parseImage :: Parser Element
parseImage = do
    parseWhiteSpace
    (_, url) <- parseFlagWithAttr "image" "url"
    alt <- parseMany parseElement
    parseFlag "/image"
    return (Image (ImageType url alt))

parseSection :: Parser Element
parseSection = do
    parseWhiteSpace
    (_, title) <- parseFlagWithAttr "section" "title"
    elems <- parseMany parseElement
    parseFlag "/section"
    return (Section (SectionType title (filterWhiteSpace elems)))

--

isWhiteSpace :: Element -> Bool
isWhiteSpace (Text str) = all (`elem` " \n\t") str
isWhiteSpace _ = False

filterWhiteSpace ::[Element] -> [Element]
filterWhiteSpace [] = []
filterWhiteSpace (elem:xs)
    | isWhiteSpace elem = filterWhiteSpace xs
    | otherwise = elem : filterWhiteSpace xs

---

parseElement :: Parser Element
parseElement = do
    elem <- parseBold <|> parseItalic <|> parseCode <|> parseList <|>
        parseParagraph <|> parseCodeBlock <|> parseLink <|> parseImage <|>
        parseSection <|> parseText
    return elem

parseBody :: Parser Body
parseBody = do
    parseFlag "body"
    elems <- parseMany parseElement
    parseFlag "/body"
    return (Body elems)

------------

parseXmlDocument :: Parser Document
parseXmlDocument = do
    parseFlag "document"
    parseWhiteSpace
    header <- parseHeader
    parseWhiteSpace
    body <- parseBody
    parseWhiteSpace
    parseFlag "/document"
    return (Document header body)

getXmlDocument :: String -> Maybe Document
getXmlDocument s = case runParser parseXmlDocument s of
    Just (doc, "") -> Just doc
    _ -> Nothing
    