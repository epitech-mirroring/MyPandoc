{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- ParserJSONHeader
-}

module JSON.ParserJSONHeader (
        parseJSONHeaderTitle,
        parseJSONHeaderAuthor,
        parseJSONHeaderDate,
        parseJSONHeaderElement,
        parseJSONHeaderElements,
        countJSONHeaderElements,
        countJSONHeaderElementsTitle,
        countJSONHeaderElementsAuthor,
        countJSONHeaderElementsDate,
        checkJSONHeader,
        parseJSONHeader
    ) where

import DataStruct (
        Header(..),
        HeaderElement(..)
    )

import ParserData (
        Parser(..),
        parseChar,
        parseString,
        parseSome,
        parseNotChar,
        parseEmptyString
    )

import Control.Applicative (Alternative(..))

parseJSONHeaderTitle :: Parser HeaderElement
parseJSONHeaderTitle = do
    _ <- parseString "\"title\":"
    _ <- parseChar '\"'
    title <- parseSome (parseNotChar '\"') <|> parseEmptyString '*'
    _ <- parseChar '\"'
    return (Title title)

parseJSONHeaderAuthor :: Parser HeaderElement
parseJSONHeaderAuthor = do
    _ <- parseString "\"author\":"
    _ <- parseChar '\"'
    author <- parseSome (parseNotChar '\"') <|> parseEmptyString '*'
    _ <- parseChar '\"'
    return (Author author)

parseJSONHeaderDate :: Parser HeaderElement
parseJSONHeaderDate = do
    _ <- parseString "\"date\":"
    _ <- parseChar '\"'
    date <- parseSome (parseNotChar '\"') <|> parseEmptyString '*'
    _ <- parseChar '\"'
    return (Date date)

parseJSONHeaderElement :: Parser HeaderElement
parseJSONHeaderElement = do
    element <- parseJSONHeaderTitle <|> parseJSONHeaderAuthor
        <|> parseJSONHeaderDate
    return element

parseJSONHeaderElements :: Char -> Parser [HeaderElement]
parseJSONHeaderElements c = do
    element <- parseJSONHeaderElement
    x <- parseChar ',' <|> parseChar c
    case x of
        ',' -> do
            elements <- parseJSONHeaderElements c
            return (element : elements)
        _ -> return [element]

countJSONHeaderElements :: [HeaderElement] -> Int
countJSONHeaderElements [] = 0
countJSONHeaderElements (_ : xs) = 1 + countJSONHeaderElements xs

countJSONHeaderElementsTitle :: [HeaderElement] -> Int
countJSONHeaderElementsTitle [] = 0
countJSONHeaderElementsTitle ((Title _) : xs) =
    1 + countJSONHeaderElementsTitle xs
countJSONHeaderElementsTitle (_ : xs) =
    countJSONHeaderElementsTitle xs

countJSONHeaderElementsAuthor :: [HeaderElement] -> Int
countJSONHeaderElementsAuthor [] = 0
countJSONHeaderElementsAuthor ((Author _) : xs) =
    1 + countJSONHeaderElementsAuthor xs
countJSONHeaderElementsAuthor (_ : xs) =
    countJSONHeaderElementsAuthor xs

countJSONHeaderElementsDate :: [HeaderElement] -> Int
countJSONHeaderElementsDate [] = 0
countJSONHeaderElementsDate ((Date _) : xs) =
    1 + countJSONHeaderElementsDate xs
countJSONHeaderElementsDate (_ : xs) =
    countJSONHeaderElementsDate xs

checkJSONHeader :: [HeaderElement] -> Bool
checkJSONHeader [] = False
checkJSONHeader header =
    let nbElem = countJSONHeaderElements header
        nbTitle = countJSONHeaderElementsTitle header
        nbAuthor = countJSONHeaderElementsAuthor header
        nbDate = countJSONHeaderElementsDate header
    in if nbElem <= 3 && nbTitle == 1 && nbAuthor <= 1 && nbDate <= 1
        then True
        else False

parseJSONHeader :: Parser Header
parseJSONHeader = do
    _ <- parseString "\"header\":{"
    elements <- parseJSONHeaderElements '}'
    if checkJSONHeader elements
        then return (Header elements)
        else Parser (\_ -> Nothing)
