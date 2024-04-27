{-
-- EPITECH PROJECT, 2024
-- My Pandoc
-- File description:
-- HandleArgs
-}

module OpenFile (
        getOption
    ) where

import System.IO (hPutStrLn, stderr)
import HandleArgs (
        Options(..),
        App(..),
        parseArgs,
    )
import Data.Maybe
import GHC.IO.Exception
import System.Exit
import Control.Exception
import Error (handleError)

openFile :: IO App
openFile = do
    let args = parseArgs
    app <- args
    let options = opt app
    let input = fromMaybe "" (oInput options)
    if input == ""
        then hPutStrLn stderr "Error: no input file" >> putStrLn input >>
            exitWith (ExitFailure 84)
        else do
            file <- readFile input
            return App { opt = options, content = file }
        `catch` (\e -> handleError e "Error: failed to read input file" >>
            return App { opt = options, content = "" })

getFormat :: String -> String
getFormat input
    | (x:_) <- input, x == '<' = "XML"
    | (x:_) <- input, x == '{' = "JSON"
    | (x:_) <- input, x == '-' = "MD"
    | otherwise = "unknown"

getOutput :: String -> String
getOutput out = if out == "" then "stdout" else out

getOption :: IO App
getOption = do
    let app = openFile
    app' <- app
    let options = opt app'
    let format = getFormat (content app')
    let output = getOutput (fromMaybe "" (oOutput options))
    return App {
        opt = options { oIformat = Just format, oOutput = Just output},
        content = content app'
    }
