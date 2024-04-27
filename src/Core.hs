{-
-- EPITECH PROJECT, 2024
-- My Pandoc
-- File description:
-- Core
-}

module Core (
    writeTheDoc
) where

import HandleArgs (Options(..), App(..))
import OpenFile (getOption)
import Data.Maybe(fromMaybe)
import DataStruct(Document(..))
import JSON.ParserJSON(parseJSON)
import XML.ParserXML(getXMLDocument)
import XML.WriterXML(documentToXML)
import System.Exit (exitWith, ExitCode (..))
import Error (handleError)
import Control.Exception

createDoc :: String -> String -> IO ()
createDoc filecont output = if output /= "stdout"
        then writeFile output filecont `catch` (`handleError` "Error: failed to write output file")
        else putStrLn filecont


getParserContent :: App -> Maybe Document
getParserContent app = case oIformat (opt app) of
    Just "JSON" -> parseJSON (content app)
    Just "XML" -> getXMLDocument (content app)
    _ -> Nothing

writeDocString :: IO App -> Maybe Document -> Maybe String
writeDocString _ doc = case doc of
    Just doc' -> return (documentToXML doc')
    Nothing -> Nothing

writeTheDoc :: IO ()
writeTheDoc = do
    let app = getOption
    app' <- app
    let doc = getParserContent app'
    let string = writeDocString app doc
    case string of
        Just str -> createDoc str (fromMaybe "stdout" (oOutput (opt app')))
        Nothing -> exitWith (ExitFailure 84)

-- TODO case insisitve
