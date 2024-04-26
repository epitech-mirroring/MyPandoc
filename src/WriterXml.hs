{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- WriterXml
-}

module WriterXml (
        documentToXml
    ) where
      
import DataStruct
import Prelude

printIndented :: Int -> String
printIndented n = "\n" ++ concat (replicate n "\t")

documentToXml :: Document -> String
documentToXml (Document header body) = "<document>" ++ headerToXml header ++
    bodyToXml body ++ "</document>"

headerToXml :: Header -> String
headerToXml (Header contents) = printIndented 1 ++ "<header title=\"" ++
    getTitle contents ++ "\">" ++ headerElementsToXml contents ++
    printIndented 1 ++ "</header>"

getTitle :: [HeaderElement] -> String
getTitle [] = ""
getTitle ((Title title):_) = title
getTitle (_:xs) = getTitle xs

headerElementsToXml :: [HeaderElement] -> String
headerElementsToXml [] = ""
headerElementsToXml ((Title title):xs) = headerElementsToXml xs
headerElementsToXml ((Author author):xs) = printIndented 2 ++ "<author>" ++
    author ++ "</author>" ++ headerElementsToXml xs
headerElementsToXml ((Date date):xs) = printIndented 2 ++ "<date>" ++ date ++
    "</date>" ++ headerElementsToXml xs

bodyToXml :: Body -> String
bodyToXml (Body content) = printIndented 1 ++ "<body>" ++
    elementsToXml content 2 ++ printIndented 1 ++ "</body>" ++ printIndented 0

elementsToXml :: [Element] -> Int -> String
elementsToXml [] _ = ""
elementsToXml (x:xs) n = elementToXml x n ++ elementsToXml xs n

elementToXml :: Element -> Int -> String
elementToXml (Text str) n = str
elementToXml (Bold elem) n = "<bold>" ++ elementToXml elem (n + 1) ++ "</bold>"
elementToXml (Italic elem) n = "<italic>" ++ elementToXml elem (n + 1)
    ++ "</italic>"
elementToXml (Code elem) n = "<code>" ++ elementToXml elem (n + 1) ++ "</code>"
elementToXml (List elems) n = printIndented n ++ "<list>" ++
    elementsToXml elems (n + 1) ++ printIndented n ++ "</list>"
elementToXml (Paragraph elems) n = printIndented n ++ "<paragraph>" ++
    elementsToXml elems (n + 1) ++ "</paragraph>"
elementToXml (CodeBlock elems) n = printIndented n ++ "<codeblock>" ++
    elementsToXml elems (n + 1) ++ printIndented n ++ "</codeblock>"
elementToXml (Link link) n = "<link url=\"" ++
    linkUrl link ++ "\">" ++ elementsToXml (linkContent link) (n + 1) ++
    "</link>"
elementToXml (Image img) n = "<image url=\"" ++
    imgUrl img ++ "\">" ++ elementsToXml (imgAlt img) (n + 1) ++ "</image>"
elementToXml (Section section) n = printIndented n ++ "<section title=\"" ++
    sectionTitle section ++ "\">" ++
    elementsToXml (sectionContent section) (n + 1) ++ printIndented n ++
    "</section>"
