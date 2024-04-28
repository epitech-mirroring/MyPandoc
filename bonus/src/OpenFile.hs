{-
-- EPITECH PROJECT, 2024
-- My Pandoc
-- File description:
-- HandleArgs
-}

module OpenFile (
        getOption,
        openFile,
        wichFormat,
        getFormat,
        getOutput
    ) where

import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode(..))
import Control.Exception (catch)
import Error (handleError)
import HandleArgs (
        Options(..),
        App(..),
        parseArgs,
    )
import JSON.ParserJSON (parseJSON)
import XML.ParserXML (getXMLDocument)
import Data.Maybe (isJust)

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
wichFormat contentfile | isJust (getXMLDocument contentfile) = "xml"
wichFormat contentfile | isJust (parseJSON contentfile) = "json"
wichFormat contentfile | take 3 contentfile == "---" = "markdown"
wichFormat _ = ""

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
