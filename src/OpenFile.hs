{-
-- EPITECH PROJECT, 2024
-- My Pandoc
-- File description:
-- HandleArgs
-}

module OpenFile (
        getOption,
        printOptions
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

openFile ::IO App
openFile = do
    let args =  parseArgs
    app <- args
    let options = opt app
    let input = fromMaybe "" (oInput options)
    if input == "" then
        hPutStrLn stderr "Error: no input file" >>
            putStrLn input >>
            exitWith (ExitFailure 84)
    else do
        file <- readFile input
        return App {
            opt = options,
            content = file
        }

getFormat :: String -> String
getFormat input
    | (x:_) <- input, x == '<' = "XML"
    | (x:_) <- input, x == '{' = "JSON"
    | (x:_) <- input, x == '-' = "MD"
    | otherwise = "unknown"

getOption :: IO App
getOption = do
    let app = openFile
    app' <- app
    let options = opt app'
    let format = getFormat (content app')
    return App {
        opt = options { oIformat = Just format },
        content = content app'
    }

printOptions :: App -> IO ()
printOptions app = do
    let options = opt app
    putStrLn "Input Format: " >> putStrLn (fromMaybe "" (oIformat options))
    putStrLn "Output Format: " >> putStrLn (fromMaybe "" (oOformat options))
    putStrLn "Output Path: " >> putStrLn (fromMaybe "" (oOutput options))
    putStrLn "Input Path: " >> putStrLn (fromMaybe "" (oInput options))
