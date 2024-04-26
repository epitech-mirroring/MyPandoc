{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- ParserXMLUtils
-}

module XML.ParserXMLUtils (
        parseXMLFlag,
        parseXMLFlagWithAttr,
        isWhiteSpace,
        filterWhiteSpace
    ) where

import ParserData (
        Parser(..),
        parseChar,
        parseString,
        parseMany,
        parseNotChar
    )

import DataStruct (Element(..))

parseXMLFlag :: String -> Parser String
parseXMLFlag flag =
    parseChar '<' >>
    parseString flag >>
    parseChar '>' >>
    return flag

parseXMLFlagWithAttr :: String -> String -> Parser (String, String)
parseXMLFlagWithAttr flag key = do
    _ <- parseChar '<'
    _ <- parseString flag
    _ <- parseChar ' '
    _ <- parseString key
    _ <- parseString "=\""
    attr <- parseMany (parseNotChar '"')
    _ <- parseString "\">"
    return (flag, attr)

isWhiteSpace :: Element -> Bool
isWhiteSpace (Text str) = all (`elem` " \n\t") str
isWhiteSpace _ = False

filterWhiteSpace ::[Element] -> [Element]
filterWhiteSpace [] = []
filterWhiteSpace (elem:xs)
    | isWhiteSpace elem = filterWhiteSpace xs
    | otherwise = elem : filterWhiteSpace xs
