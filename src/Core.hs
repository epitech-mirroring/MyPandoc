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

createDoc :: String -> String -> IO ()
createDoc filecont output = if output /= "stdout"
    then writeFile output filecont
    else putStrLn filecont

{-//TODO
    - Add the function to send the content of the file to paser
    - Add the function to get the parsed content from writter
-}

writeTheDoc :: IO ()
writeTheDoc = do
    let app = getOption
    app' <- app
    createDoc (content app') (fromMaybe "" (oOutput (opt app')))
