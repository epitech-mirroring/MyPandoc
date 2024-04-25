{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- ParserXml
-}

module ParserXml (
        parseHeader
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
    (if arg == "/header" then return (Header title (Just author) Nothing)
    else do
        parseWhiteSpace
        date <- parseMany (parseNotChar '<')
        parseFlag "/date" *> parseWhiteSpace *> parseFlag "/header"
        return (Header title (Just author) (Just date)))
parseHeaderAuthorAndDate title "date" = do
    date <- parseMany (parseNotChar '<')
    parseAnd (parseFlag "/date") parseWhiteSpace
    arg <- parseFlag "/header" <|> parseFlag "author"
    (if arg == "/header" then return (Header title Nothing (Just date))
    else do
        parseWhiteSpace
        author <- parseMany (parseNotChar '<')
        parseFlag "/author" *> parseWhiteSpace *> parseFlag "/header"
        return (Header title (Just author) (Just date)))

parseHeader :: Parser Header
parseHeader = do
    parseWhiteSpace
    (_, title) <- parseFlagWithAttr "header" "title"
    parseWhiteSpace
    arg <- parseFlag "/header" <|> parseFlag "author" <|> parseFlag "date"
    if arg == "/header" then
        return (Header title Nothing Nothing)
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
    parseFlag "bold"
    elem <- parseElement
    parseFlag "/bold"
    return (Bold elem)

parseItalic :: Parser Element
parseItalic = do
    parseFlag "italic"
    elem <- parseElement
    parseFlag "/italic"
    return (Italic elem)

parseCode :: Parser Element
parseCode = do
    parseFlag "code"
    elem <- parseElement
    parseFlag "/code"
    return (Code elem)

parseList :: Parser Element
parseList = do
    parseFlag "list"
    elems <- parseMany parseElement
    parseFlag "/list"
    return (List elems)

parseParagraph :: Parser Element
parseParagraph = do
    parseFlag "paragraph"
    elems <- parseMany parseElement
    parseFlag "/paragraph"
    return (Paragraph elems)

parseCodeBlock :: Parser Element
parseCodeBlock = do
    parseFlag "codeblock"
    elems <- parseMany parseElement
    parseFlag "/codeblock"
    return (CodeBlock elems)

parseLink :: Parser Element
parseLink = do
    (_, url) <- parseFlagWithAttr "link" "url"
    elems <- parseMany parseElement
    parseFlag "/link"
    return (Link (LinkType url elems))

parseImage :: Parser Element
parseImage = do
    (_, url) <- parseFlagWithAttr "image" "url"
    alt <- parseMany parseElement
    parseFlag "/image"
    return (Image (ImageType url alt))

parseSection :: Parser Element
parseSection = do
    (_, title) <- parseFlagWithAttr "section" "title"
    elems <- parseMany parseElement
    parseFlag "/section"
    return (Section (SectionType title elems))
    
---

parseElement :: Parser Element
parseElement = do
    parseWhiteSpace
    elem <- parseBold <|> parseItalic <|> parseCode <|> parseList <|>
        parseParagraph <|> parseCodeBlock <|> parseLink <|> parseImage <|>
        parseSection <|> parseText
    parseWhiteSpace
    return elem

parseBody :: Parser Body
parseBody = do
    parseFlag "body"
    elems <- parseMany parseElement
    parseFlag "/body"
    return (Body elems)
    
------------

parseDocument :: Parser Document
parseDocument = do
    parseFlag "document"
    parseWhiteSpace
    header <- parseHeader
    parseWhiteSpace
    body <- parseBody
    parseWhiteSpace
    parseFlag "/document"
    return (Document header body)
