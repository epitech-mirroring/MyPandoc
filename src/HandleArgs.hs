{-
-- EPITECH PROJECT, 2024
-- My Pandoc
-- File description:
-- HandleArgs
-}

module HandleArgs (
        Options(..),
        defaultOptions,
        options,
        App(..),
        getHelp,
        checkArgs,
        parseOptions,
        parseArgs
    ) where

import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import Data.Maybe (isJust)

data Options = Options {
        oIformat :: Maybe String,
        oOformat :: Maybe String,
        oOutput :: Maybe String,
        oInput :: Maybe String
    } deriving (Show, Eq)

data App = App {
    opt :: Options,
    content :: String
}

defaultOptions :: Options
defaultOptions = Options {
    oIformat = Nothing,
    oOformat = Nothing,
    oOutput = Nothing,
    oInput = Nothing
}

{-
input path (Mandatory) to read the data from the right file (ifile)
output path (option) to write the data in the right file or standard output
output format (Mandatory) to write the right format of data
input format (optional) if not launch detection of the format
-}
options :: [OptDescr (Options -> Options)]
options = [
    Option ['i'] [""] (ReqArg (\i opts -> opts {oInput = Just i}) "ifile")
        "path to the file to convert",
    Option ['o'] [""] (ReqArg (\o opts -> opts {oOutput = Just o}) "ofile")
        "output format (xml, json, markdown)",
    Option ['f'] [""] (ReqArg (\f opts -> opts {oOformat = Just f}) "oformat")
        "path to the output file",
    Option ['e'] [""] (ReqArg (\e opts -> opts {oIformat = Just e}) "iformat")
        "input format (xml, json, markdown)"
    ]

getHelp :: String
getHelp =
    usageInfo "USAGE: ./mypandoc\
    \-i ifile -f oformat [-o ofile] [-e iformat]" options

checkArgs :: Options -> Maybe Options
checkArgs Options {oOformat = Nothing} = Nothing
checkArgs Options {oInput = Nothing} = Nothing
checkArgs opts = Just (opts{
    oOutput = if isJust (oOutput opts) then oOutput opts
        else Nothing,
    oIformat = if isJust (oIformat opts) then oIformat opts
        else Nothing,
    oOformat = if isJust (oOformat opts) then oOformat opts
        else Nothing,
    oInput = if isJust (oInput opts) then oInput opts
        else Nothing
})

parseOptions :: [String] -> IO Options
parseOptions args = case getOpt Permute options args of
    (o, [], []) -> foldl (>>=) (return defaultOptions) (map (return .) o)
    (_, _, errs) -> hPutStrLn stderr (concat errs) >>
        hPutStrLn stderr getHelp >>
        exitWith (ExitFailure 84)

parseArgs :: IO App
parseArgs = do
    args <- getArgs
    opts <- parseOptions args
    case checkArgs opts of
        Just finalOpts -> return App { opt = finalOpts, content = "" }
        Nothing -> hPutStrLn stderr getHelp >>
            exitWith (ExitFailure 84)
