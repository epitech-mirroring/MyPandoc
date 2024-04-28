{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- Spec
-}

import Test.Hspec
import HandleArgsSpecs (handleArgsSpecs)
import OpenFileSpecs (openFilesSpecs)

main :: IO ()
main = hspec $ do
    handleArgsSpecs
    openFilesSpecs
