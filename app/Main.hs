{-
-- EPITECH PROJECT, 2024
-- My Pandoc
-- File description:
-- Initialization module of the project
-}

module Main (main) where
import Data.Maybe (fromMaybe);

import HandleArgs (
        Options(..),
        parseArgs,
    )
main :: IO ()
main = do
    opts <- parseArgs
    case opts of
        Options {oIformat = i, oOformat = o, oOutput = p, oInput = f} -> do
            putStrLn "Hello World"
            putStrLn $ fromMaybe "" i
            putStrLn $ fromMaybe "" o
            putStrLn $ fromMaybe "" p
            putStrLn $ fromMaybe "" f
            return ()
        _ -> do
          return ()
