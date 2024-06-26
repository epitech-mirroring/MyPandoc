{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- WriterXML
-}

module XML.WriterXML (
        printIndented,
        documentToXML,
        headerToXML,
        headerElementsToXML,
        bodyToXML,
        elementsToXML,
        elementToXML
    ) where

import DataStruct (
        Document(..),
        Header(..),
        HeaderElement(..),
        Body(..),
        Element(..),
        LinkType(..),
        ImageType(..),
        SectionType(..)
    )

printIndented :: Int -> String
printIndented n
    | n < 0 = ""
    | otherwise = "\n" ++ replicate (n * 4) ' '

documentToXML :: Document -> String
documentToXML (Document header body) = "<document>" ++ headerToXML header ++
    bodyToXML body ++ "</document>"

headerToXML :: Header -> String
headerToXML (Header contents) = printIndented 1 ++ "<header title=\"" ++
    getTitle contents ++ "\">" ++ headerElementsToXML contents ++
    printIndented 1 ++ "</header>"

getTitle :: [HeaderElement] -> String
getTitle [] = ""
getTitle ((Title title):_) = title
getTitle (_:xs) = getTitle xs

headerElementsToXML :: [HeaderElement] -> String
headerElementsToXML [] = ""
headerElementsToXML ((Title title):xs) = headerElementsToXML xs
headerElementsToXML ((Author author):xs) = printIndented 2 ++ "<author>" ++
    author ++ "</author>" ++ headerElementsToXML xs
headerElementsToXML ((Date date):xs) = printIndented 2 ++ "<date>" ++ date ++
    "</date>" ++ headerElementsToXML xs

bodyToXML :: Body -> String
bodyToXML (Body content) = printIndented 1 ++ "<body>" ++
    elementsToXML content 2 ++ printIndented 1 ++ "</body>" ++ printIndented 0

elementsToXML :: [Element] -> Int -> String
elementsToXML [] _ = ""
elementsToXML (x:xs) n = elementToXML x n ++ elementsToXML xs n

elementToXML :: Element -> Int -> String
elementToXML e (-1) = elementToXML e (-2)
elementToXML (Text str) _ = str
elementToXML (Bold elem) _ = "<bold>" ++ elementToXML elem (-1) ++ "</bold>"
elementToXML (Italic elem) _ = "<italic>" ++ elementToXML elem (-1)
    ++ "</italic>"
elementToXML (Code elem) _ = "<code>" ++ elementToXML elem (-1) ++ "</code>"
elementToXML (List elems) n = printIndented n ++ "<list>" ++
    elementsToXML elems (n + 1) ++ printIndented n ++ "</list>"
elementToXML (Paragraph elems) n = printIndented n ++ "<paragraph>" ++
    elementsToXML elems (-1) ++ "</paragraph>"
elementToXML (CodeBlock elems) n = printIndented n ++ "<codeblock>" ++
    elementsToXML elems (-1) ++ "</codeblock>"
elementToXML (Link link) _ = "<link url=\"" ++
    linkUrl link ++ "\">" ++ elementsToXML (linkContent link) (-1) ++
    "</link>"
elementToXML (Image img) _ = "<image url=\"" ++
    imgUrl img ++ "\">" ++ elementsToXML (imgAlt img) (-1) ++ "</image>"
elementToXML (Section section) n = printIndented n ++ "<section title=\"" ++
    sectionTitle section ++ "\">" ++
    elementsToXML (sectionContent section) (n + 1) ++ printIndented n ++
    "</section>"
elementToXML (Empty) _ = ""
