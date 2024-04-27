{-
-- EPITECH PROJECT, 2024
-- My Pandoc
-- File description:
-- HandleArgs
-}

module Error (
    handleError
) where

import System.Exit (exitWith, ExitCode(..))
import Control.Exception

handleError :: IOException -> String -> IO ()
handleError _ message = putStrLn message >> exitWith (ExitFailure 84)
