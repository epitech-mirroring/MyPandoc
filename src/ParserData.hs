{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- ParserData
-}

module ParserData (
        Parser(..)
    ) where


import Data.Char ()
import Control.Applicative (Alternative(..))
import Control.Monad ()


data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
    fmap fct parser = Parser $ \str -> case runParser parser str of
        Just (a, s) -> Just (fct a, s)
        Nothing -> Nothing

instance Applicative Parser where
    pure a = Parser $ \str -> Just (a, str)
    parser1 <*> parser2 = Parser $ \str -> case runParser parser1 str of
        Just (fct, s) -> case runParser parser2 s of
            Just (a, ss) -> Just (fct a, ss)
            Nothing -> Nothing
        Nothing -> Nothing
    parser1 *> parser2 = Parser $ \str -> case runParser parser1 str of
        Just (_, s) -> runParser parser2 s
        Nothing -> Nothing
    parser1 <* parser2 = Parser $ \str -> case runParser parser1 str of
        Just (a, s) -> case runParser parser2 s of
            Just (_, ss) -> Just (a, ss)
            Nothing -> Nothing
        Nothing -> Nothing

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    parser1 <|> parser2 = Parser $ \str -> case runParser parser1 str of
        Just (a, s) -> Just (a, s)
        Nothing -> runParser parser2 str

instance Monad Parser where
    return = pure
    parser >>= fct = Parser $ \str -> case runParser parser str of
        Just (a, s) -> runParser (fct a) s
        Nothing -> Nothing
