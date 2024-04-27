{-
-- EPITECH PROJECT, 2024
-- My Pandoc
-- File description:
-- Initialization module of the project
-}

module Main (main) where
import GHC.IO.Device (RawIO(write))
import Core (writeTheDoc)

main :: IO ()
main = writeTheDoc