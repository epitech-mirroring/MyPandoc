{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- ParserJSON
-}

module JSON.ParserJSON (
        unbeautifyJSON,
        parseJSONBody,
        parseJSONDocument,
        parseJSON
    ) where

import DataStruct (Document(..), Body(..))
import ParserData (Parser(..), parseChar, parseString, parseEmptyList)
import JSON.ParserJSONHeader (parseJSONHeader)
import JSON.ParserJSONElements (parseJSONElements)
import Control.Applicative ((<|>))

unbeautifyJSON :: String -> Bool -> String
unbeautifyJSON [] _ = []
unbeautifyJSON ('\"' : xs) False = '\"' : unbeautifyJSON xs True
unbeautifyJSON (x : xs) False = x : unbeautifyJSON xs False
unbeautifyJSON ('\"' : xs) True = '\"' : unbeautifyJSON xs False
unbeautifyJSON (' ' : xs) True = unbeautifyJSON xs True
unbeautifyJSON ('\n' : xs) True = unbeautifyJSON xs True
unbeautifyJSON ('\t' : xs) True = unbeautifyJSON xs True
unbeautifyJSON (x : xs) True = x : unbeautifyJSON xs True

parseJSONBody :: Parser Body
parseJSONBody = do
    _ <- parseString "\"body\":"
    _ <- parseChar '['
    elements <- parseJSONElements ']' <|> parseEmptyList ']'
    return (Body elements)

parseJSONDocument :: Parser Document
parseJSONDocument = do
    _ <- parseChar '{'
    headerContent <- parseJSONHeader
    _ <- parseChar ','
    bodyContent <- parseJSONBody
    _ <- parseChar '}'
    return (Document headerContent bodyContent)

parseJSON :: String -> Maybe Document
parseJSON str = case runParser parseJSONDocument (unbeautifyJSON str True) of
    Just (doc, "") -> Just doc
    Just (_, _) -> Nothing
    Nothing -> Nothing
