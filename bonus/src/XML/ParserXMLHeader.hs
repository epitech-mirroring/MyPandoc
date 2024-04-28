{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- ParserXMLHeader
-}

module XML.ParserXMLHeader (
        parseXMLHeader,
        parseXMLHeaderAuthorAndDate
    ) where

import ParserData (
        Parser(..),
        parseAnd,
        parseWhiteSpace,
        parseMany,
        parseNotChar
    )

import DataStruct (Header(..), HeaderElement(..))
import XML.ParserXMLUtils (parseXMLFlag, parseXMLFlagWithAttr)
import Control.Applicative (Alternative(..))

parseXMLHeaderAuthorAndDate :: String -> String -> Parser Header
parseXMLHeaderAuthorAndDate title "author" = do
    author <- parseMany (parseNotChar '<')
    _ <- parseAnd (parseXMLFlag "/author") parseWhiteSpace
    arg <- parseXMLFlag "date" <|> parseXMLFlag "/header"
    (if arg == "/header" then return (Header [Title title, Author author])
    else do
        _ <- parseWhiteSpace
        date <- parseMany (parseNotChar '<')
        _ <- parseXMLFlag "/date" *> parseWhiteSpace *> parseXMLFlag "/header"
        return (Header [Title title, Author author, Date date]))
parseXMLHeaderAuthorAndDate title "date" = do
    date <- parseMany (parseNotChar '<')
    _ <- parseAnd (parseXMLFlag "/date") parseWhiteSpace
    arg <- parseXMLFlag "/header" <|> parseXMLFlag "author"
    (if arg == "/header" then return (Header [Title title, Date date]) else do
        _ <- parseWhiteSpace
        author <- parseMany (parseNotChar '<')
        _ <- parseXMLFlag "/author" *> parseWhiteSpace *>
            parseXMLFlag "/header"
        return (Header [Title title, Author author, Date date]))
parseXMLHeaderAuthorAndDate _ _ = Parser (\_ -> Nothing)

parseXMLHeader :: Parser Header
parseXMLHeader = do
    _ <- parseWhiteSpace
    (_, title) <- parseXMLFlagWithAttr "header" "title"
    _ <- parseWhiteSpace
    arg <- parseXMLFlag "/header" <|> parseXMLFlag "author"
        <|> parseXMLFlag "date"
    if arg == "/header" then
        return (Header [Title title])
    else
        parseWhiteSpace >> parseXMLHeaderAuthorAndDate title arg
