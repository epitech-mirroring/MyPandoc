{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- ParserData
-}

module ParserData (
        Parser(..),
        parseChar,
        parseAnyChar,
        parseOr,
        parseAnd,
        parseAndWith,
        parseMany,
        parseSome,
        parseUInt,
        parseInt,
        parseTuple,
        parseTruple,
        parseString,
        parseEmpty,
        parseEmptyString,
        parseEmptyList,
        parseWhiteSpace,
        parseNotChar
    ) where

import DataStruct (
        Element(..)
    )

import Data.Char (isDigit)
import Control.Applicative (Alternative(..))
import Control.Monad ()

data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
    fmap fct parser = Parser $ \str -> case runParser parser str of
        Just (a, str) -> Just (fct a, str)
        Nothing -> Nothing

instance Applicative Parser where
    pure a = Parser $ \str -> Just (a, str)
    parser1 <*> parser2 = Parser $ \str -> case runParser parser1 str of
        Just (fct, str) -> case runParser parser2 str of
            Just (a, str) -> Just (fct a, str)
            Nothing -> Nothing
        Nothing -> Nothing
    parser1 *> parser2 = Parser $ \str -> case runParser parser1 str of
        Just (_, str) -> runParser parser2 str
        Nothing -> Nothing
    parser1 <* parser2 = Parser $ \str -> case runParser parser1 str of
        Just (a, str) -> case runParser parser2 str of
            Just (_, str) -> Just (a, str)
            Nothing -> Nothing
        Nothing -> Nothing

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    parser1 <|> parser2 = Parser $ \str -> case runParser parser1 str of
        Just (a, str) -> Just (a, str)
        Nothing -> runParser parser2 str

instance Monad Parser where
    return = pure
    parser >>= fct = Parser $ \str -> case runParser parser str of
        Just (a, str) -> runParser (fct a) str
        Nothing -> Nothing

parseChar :: Char -> Parser Char
parseChar c = Parser p
    where
        p "" = Nothing
        p (x:xs)
            | c == x = Just (c, xs)
            | otherwise = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar str = Parser p
    where
        p "" = Nothing
        p (x:xs)
            | x `elem` str = Just (x, xs)
            | otherwise = Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 = Parser p
    where
        p s = case runParser p1 s of
            Just p -> Just p
            Nothing -> runParser p2 s

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 = Parser p
    where
        p s = case runParser p1 s of
            Just (res1, s1) -> case runParser p2 s1 of
                Just (res2, s2) -> Just ((res1, res2), s2)
                Nothing -> Nothing
            Nothing -> Nothing

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 = fmap (uncurry f) (parseAnd p1 p2)

parseMany :: Parser a -> Parser [a]
parseMany p1 = Parser p
    where
        p "" = Just ([], "")
        p s = case runParser p1 s of
            Just (res, s1) -> case runParser (parseMany p1) s1 of
                Just (res1, s2) -> Just (res:res1, s2)
                Nothing -> Just ([res], s1)
            Nothing -> Just ([], s)

parseSome :: Parser a -> Parser [a]
parseSome p1 = Parser p
    where
        p s = case runParser (parseAnd p1 (parseMany p1)) s of
            Just ((res, res1), s1) -> Just (res:res1, s1)
            Nothing -> Nothing

parseUInt :: Parser Int
parseUInt = Parser p
    where
        p s = case runParser (parseSome (parseAnyChar "0123456789")) s of
            Just (res, s1) -> Just (read res, s1)
            Nothing -> Nothing

parseInt :: Parser Int
parseInt = Parser p
    where
        p s = runParser (parseOr (parseAndWith (\ _ y -> (-1) * y)
          (parseChar '-') parseUInt) parseUInt) s

parseTuple :: Parser a -> Parser (a, a)
parseTuple p1 = Parser p
  where
      p s = runParser (parseAndWith
        (\ _ (res1, (_, (res2, _))) -> (res1, res2))
        (parseChar '(') (parseAnd p1 (parseAnd (parseChar ',')
        (parseAnd p1 (parseChar ')'))))) s

parseTruple :: Parser (Int , Int , Int)
parseTruple = do
  _ <- parseChar '('
  a <- parseInt
  _ <- parseChar ','
  b <- parseInt
  _ <- parseChar ','
  c <- parseInt
  _ <- parseChar ')'
  return (a, b, c)

parseString :: String -> Parser String
parseString [] = pure []
parseString (x:xs) = (:) <$> parseChar x <*> parseString xs

parseEmpty :: Char -> Parser Element
parseEmpty '*' = return Empty
parseEmpty c = do
    _ <- parseChar c
    return Empty

parseEmptyString :: Char -> Parser String
parseEmptyString '*' = return ""
parseEmptyString c = do
    _ <- parseChar c
    return ""

parseEmptyList :: Char -> Parser [Element]
parseEmptyList '*' = return []
parseEmptyList c = do
    _ <- parseChar c
    return []

parseWhiteSpace :: Parser String
parseWhiteSpace = parseMany (parseAnyChar " \n\t")

parseNotChar :: Char -> Parser Char
parseNotChar c = Parser p
    where
        p "" = Nothing
        p (x:xs)
            | c /= x = Just (x, xs)
            | otherwise = Nothing
