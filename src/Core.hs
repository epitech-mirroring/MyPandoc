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
import Data.Maybe (fromMaybe)
import DataStruct (Document(..))
import JSON.ParserJSON (parseJSON)
import XML.ParserXML (getXMLDocument)
import XML.WriterXML (documentToXML)
import Error (handleError)
import Markdown.WriterMarkdown (writeMarkdownDocument)
import JSON.WriterJSON (documentToJSON)
import Control.Exception (catch)

createDoc :: String -> String -> IO ()
createDoc filecont output = if output /= "stdout"
        then writeFile output filecont `catch`
            (`handleError` "Error: failed to write output file")
        else putStrLn filecont

getParserContent :: App -> Maybe Document
getParserContent app = case oIformat (opt app) of
    Just "json" -> parseJSON (content app)
    Just "xml" -> getXMLDocument (content app)
    _ -> Nothing

writeDocString :: App -> Maybe Document -> Maybe String
writeDocString (App (Options _ (Just "xml") _ _) _)
    (Just doc) = Just (documentToXML doc)
writeDocString (App (Options _ (Just "json") _ _) _)
    (Just doc) = Just (documentToJSON doc)
writeDocString (App (Options _ (Just "markdown") _ _) _)
    (Just doc) = Just (writeMarkdownDocument doc)
writeDocString _ _ = Nothing

writeTheDoc :: IO ()
writeTheDoc = do
    let app = getOption
    app' <- app
    let doc = getParserContent app'
    let string = writeDocString app' doc
    case string of
        Just str -> createDoc str (fromMaybe "stdout" (oOutput (opt app')))
        Nothing -> handleError
            (userError "Error: invalid format") "Error: invalid format"

-- TODO case insisitve
