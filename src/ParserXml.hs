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
        parseFlag "/date"
        return (Header title (Just author) (Just date)))
parseHeaderAuthorAndDate title "date" = do
    date <- parseMany (parseNotChar '<')
    parseAnd (parseFlag "/date") parseWhiteSpace
    arg <- parseFlag "/header" <|> parseFlag "author"
    (if arg == "/header" then return (Header title Nothing (Just date))
    else do
        parseWhiteSpace
        author <- parseMany (parseNotChar '<')
        parseFlag "/author"
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
