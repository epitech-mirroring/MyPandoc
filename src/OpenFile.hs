{-
-- EPITECH PROJECT, 2024
-- My Pandoc
-- File description:
-- HandleArgs
-}

module OpenFile (
        getOption,
        wichFormat,
        openFile,
        getFormat,
        getOutput
    ) where

import System.IO (hPutStrLn, stderr)
import HandleArgs (
        Options(..),
        App(..),
        parseArgs,
    )

import System.Exit (exitWith, ExitCode(..))
import Control.Exception (catch)
import Error (handleError)

openFile :: IO App
openFile = do
    App options _ <- parseArgs
    case oInput options of
        Nothing -> hPutStrLn stderr "Error: no input file" >>
            exitWith (ExitFailure 84)
        Just input -> do
            file <- readFile input
            return App { opt = options, content = file }
        `catch` (\e -> handleError e "Error: failed to read input file" >>
            exitWith (ExitFailure 84))

wichFormat :: String -> String
wichFormat ('<' : _) = "xml"
wichFormat ('{' : _) = "json"
wichFormat ('-' : _) = "markdown"
wichFormat _ = "unknown"

getFormat :: Maybe String -> String -> String
getFormat Nothing fileContent = wichFormat fileContent
getFormat (Just format) _ = format

getOutput :: Maybe String -> String
getOutput Nothing = "stdout"
getOutput (Just output) = output

getOption :: IO App
getOption = do
    App options fileContent <- openFile
    return App {
        opt = options {
            oIformat = Just (getFormat (oIformat options) fileContent),
            oOutput = Just (getOutput (oOutput options))
            },
        content = fileContent
    }
