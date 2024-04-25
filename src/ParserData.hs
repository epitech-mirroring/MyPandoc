{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- ParserData
-}

module ParserData (
        Parser(..)
    ) where


import Data.Char
import Control.Applicative (Alternative(..))
import Control.Monad


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
