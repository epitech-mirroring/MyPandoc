{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- ParserDataSpecs
-}

module ParserDataSpecs (
    parserDataSpecs
) where

import Test.Hspec

import ParserData (
        Parser(..),
        parseChar,
        parseAnyChar,
        parseOr,
        parseAnd,
        parseAndWith,
        parseMany,
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
    )

import DataStruct (
        Element(..)
    )

parserDataSpecs :: Spec
parserDataSpecs = do
    describe "ParserData" $ do
        parserCharSpec
        parseAnyCharSpec
        parseOrSpec
        parseAndSpec
        parseAndWithSpec
        parseManySpec
        parseUIntSpec
        parseIntSpec
        parseTupleSpec
        parseTrupleSpec
        parseStringSpec
        parseEmptySpec
        parseEmptyStringSpec
        parseEmptyListSpec
        parseWhiteSpaceSpec
        parseNotCharSpec
        applicativeParserSpec

parserCharSpec :: Spec
parserCharSpec = do
    describe "parseChar function :" $ do
        it "should return a parser that parses a char" $ do
            runParser (parseChar 'a') "a" `shouldBe` Just ('a', "")
        it "should return a parser that fails if the char is not the expected one" $ do
            runParser (parseChar 'a') "b" `shouldBe` Nothing

parseAnyCharSpec :: Spec
parseAnyCharSpec = do
    describe "parseAnyChar function :" $ do
        it "should return a parser that parses any char" $ do
            runParser (parseAnyChar "azertyuiop") "a" `shouldBe` Just ('a', "")
        it "should return a parser that fails if the string is empty" $ do
            runParser (parseAnyChar "azertop") "" `shouldBe` Nothing

parseOrSpec :: Spec
parseOrSpec = do
    describe "parseOr function :" $ do
        it "should return a parser that parses the first parser if it succeeds" $ do
            let parser = parseOr (parseChar 'a') (parseChar 'b')
            runParser parser "a" `shouldBe` Just ('a', "")
        it "should return a parser that parses the second parser if the first one fails" $ do
            let parser = parseOr (parseChar 'a') (parseChar 'b')
            runParser parser "b" `shouldBe` Just ('b', "")
        it "should return a parser that fails if both parsers fail" $ do
            let parser = parseOr (parseChar 'a') (parseChar 'b')
            runParser parser "c" `shouldBe` Nothing

parseAndSpec :: Spec
parseAndSpec = do
    describe "parseAnd function :" $ do
        it "should return a parser that parses both parsers" $ do
            let parser = parseAnd (parseChar 'a') (parseChar 'b')
            runParser parser "ab" `shouldBe` Just (('a', 'b'), "")
        it "should return a parser that fails if the first parser fails" $ do
            let parser = parseAnd (parseChar 'a') (parseChar 'b')
            runParser parser "b" `shouldBe` Nothing
        it "should return a parser that fails if the second parser fails" $ do
            let parser = parseAnd (parseChar 'a') (parseChar 'b')
            runParser parser "a" `shouldBe` Nothing

parseAndWithSpec :: Spec
parseAndWithSpec = do
    describe "parseAndWith function :" $ do
        it "should return a parser that parses both parsers and applies the function" $ do
            let parser = parseAndWith (\a b -> a : b : []) (parseChar 'a') (parseChar 'b')
            runParser parser "ab" `shouldBe` Just ("ab", "")
        it "should return a parser that fails if the first parser fails" $ do
            let parser = parseAndWith (\a b -> a : b : []) (parseChar 'a') (parseChar 'b')
            runParser parser "b" `shouldBe` Nothing
        it "should return a parser that fails if the second parser fails" $ do
            let parser = parseAndWith (\a b -> a : b : []) (parseChar 'a') (parseChar 'b')
            runParser parser "a" `shouldBe` Nothing

parseManySpec :: Spec
parseManySpec = do
    describe "parseMany function :" $ do
        it "should return a parser that parses 0 or more times the parser" $ do
            let parser = parseMany (parseChar 'a')
            runParser parser "aaa" `shouldBe` Just ("aaa", "")
        it "should return a parser that parses 0 times the parser" $ do
            let parser = parseMany (parseChar 'a')
            runParser parser "b" `shouldBe` Just ("", "b")
        it "should return a parser that fails if the parser fails" $ do
            let parser = parseMany (parseChar 'a')
            runParser parser "ab" `shouldBe` Just ("a", "b")

parseUIntSpec :: Spec
parseUIntSpec = do
    describe "parseUInt function :" $ do
        it "should return a parser that parses an unsigned int" $ do
            runParser parseUInt "123" `shouldBe` Just (123, "")
        it "should return a parser that fails if the string is empty" $ do
            runParser parseUInt "" `shouldBe` Nothing
        it "should return a parser that fails if the string is not an unsigned int" $ do
            runParser parseUInt "a" `shouldBe` Nothing

parseIntSpec :: Spec
parseIntSpec = do
    describe "parseInt function :" $ do
        it "should return a parser that parses an int" $ do
            runParser parseInt "-123" `shouldBe` Just (-123, "")
        it "should return a parser that fails if the string is empty" $ do
            runParser parseInt "" `shouldBe` Nothing
        it "should return a parser that fails if the string is not an int" $ do
            runParser parseInt "a" `shouldBe` Nothing

parseTupleSpec :: Spec
parseTupleSpec = do
    describe "parseTuple function :" $ do
        it "should return a parser that parses a tuple" $ do
            runParser (parseTuple (parseChar 'a')) "(a,a)" `shouldBe` Just (('a', 'a'), "")
        it "should return a parser that fails if the string is empty" $ do
            runParser (parseTuple (parseChar 'a')) "" `shouldBe` Nothing
        it "should return a parser that fails if the string is not a tuple" $ do
            runParser (parseTuple (parseChar 'a')) "(a,a" `shouldBe` Nothing

parseTrupleSpec :: Spec
parseTrupleSpec = do
    describe "parseTruple function :" $ do
        it "should return a parser that parses a int truple" $ do
            runParser parseTruple "(1,2,3)" `shouldBe` Just ((1, 2, 3), "")
        it "should return a parser that fails if the string is empty" $ do
            runParser parseTruple "" `shouldBe` Nothing
        it "should return a parser that fails if the string is not a truple" $ do
            runParser parseTruple "(1,2,3" `shouldBe` Nothing

parseStringSpec :: Spec
parseStringSpec = do
    describe "parseString function :" $ do
        it "should return a parser that parses a string" $ do
            runParser (parseString "azerty") "azerty" `shouldBe` Just ("azerty", "")
        it "should return a parser that fails if the string is empty" $ do
            runParser (parseString "azerty") "" `shouldBe` Nothing
        it "should return a parser that fails if the string is not the expected one" $ do
            runParser (parseString "azerty") "azert" `shouldBe` Nothing

parseEmptySpec :: Spec
parseEmptySpec = do
    describe "parseEmpty function :" $ do
        it "should return a Empty object" $ do
            runParser (parseEmpty '*') "" `shouldBe` Just (Empty, "")
        it "should return a Empty object" $ do
            runParser (parseEmpty 'a') "aa" `shouldBe` Just (Empty, "a")

parseEmptyStringSpec :: Spec
parseEmptyStringSpec = do
    describe "parseEmptyString function :" $ do
        it "should return a parser that parses an empty string" $ do
            runParser (parseEmptyString '*') "" `shouldBe` Just ("", "")
        it "should return a parser that fails if the string is not empty" $ do
            runParser (parseEmptyString 'a') "aa" `shouldBe` Just ("", "a")

parseEmptyListSpec :: Spec
parseEmptyListSpec = do
    describe "parseEmptyList function :" $ do
        it "should return a parser that parses an empty list" $ do
            runParser (parseEmptyList '*') "[]" `shouldBe` Just ([], "[]")
        it "should return a parser that parses an empty list" $ do
            runParser (parseEmptyList 'a') "a" `shouldBe` Just ([], "")
        it "should return a parser that fails if the list is not empty" $ do
            runParser (parseEmptyList 'a') "[a],[a]" `shouldBe` Nothing

parseWhiteSpaceSpec :: Spec
parseWhiteSpaceSpec = do
    describe "parseWhiteSpace function :" $ do
        it "should return a parser that parses a white space" $ do
            runParser parseWhiteSpace " " `shouldBe` Just (" ", "")
        it "should return a parser that fails if the string is not a white space" $ do
            runParser parseWhiteSpace "a" `shouldBe` Just ("", "a")

parseNotCharSpec :: Spec
parseNotCharSpec = do
    describe "parseNotChar function :" $ do
        it "should return a parser that parses a char that is not the expected one" $ do
            runParser (parseNotChar 'a') "b" `shouldBe` Just ('b', "")
        it "should return a parser that fails if the char is the expected one" $ do
            runParser (parseNotChar 'a') "a" `shouldBe` Nothing

applicativeParserSpec :: Spec
applicativeParserSpec = do
    describe "Applicative Parser" $ do
        it "should return a parser that parses a tuple" $ do
            let parser = (,) <$> parseChar 'a' <*> parseChar 'b'
            runParser parser "ab" `shouldBe` Just (('a', 'b'), "")
        it "should return a parser that parses a tuple" $ do
            let parser = (,) <$> parseChar 'a' <*> parseChar 'b'
            runParser parser "a" `shouldBe` Nothing
        it "should return a parser that parses a tuple" $ do
            let parser = (,) <$> parseChar 'a' <*> parseChar 'b'
            runParser parser "b" `shouldBe` Nothing
        it "should return a parser that parses a tuple" $ do
            let parser = (,) <$> parseChar 'a' <*> parseChar 'b'
            runParser parser "c" `shouldBe` Nothing
        it "should apply *> operator and return Nothing" $ do
            let parser = parseChar 'a' *> parseChar 'b'
            runParser parser "b" `shouldBe` Nothing
        it "should apply <* operator and return Nothing" $ do
            let parser = parseChar 'a' <* parseChar 'b'
            runParser parser "a" `shouldBe` Nothing
        it "should apply <* operator and return Nothing 2" $ do
            let parser = parseChar 'a' <* parseChar 'b'
            runParser parser "b" `shouldBe` Nothing
        it "should apply <* operator" $ do
            let parser = parseChar 'a' <* parseChar 'b'
            runParser parser "ab" `shouldBe` Just ('a', "")
