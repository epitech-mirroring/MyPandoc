{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- WriterJSON
-}

module JSON.WriterJSON (
        documentToJSON,
        headerToJSON,
        headerContentToJSON,
        bodyToJSON,
        listContentToJSON,
        contentToJSON,
        paragraphToJSON,
        formattedTextToJSON,
        listToJSON,
        linksToJSON,
        imagesToJSON,
        sectionToJSON
    ) where

import DataStruct (
    Document (..),
    Header (..),
    HeaderElement (..),
    Body (..),
    Element (..),
    LinkType (..),
    ImageType (..),
    SectionType (..)
    )

documentToJSON :: Document -> String
documentToJSON doc = "{\n" ++ headerToJSON (header doc)
    ++ ",\n" ++ bodyToJSON (body doc) ++ "\n}"

headerToJSON :: Header -> String
headerToJSON h = "    \"header\": {\n"
    ++ (headerContentToJSON $ contents h) ++ "\n    }"

headerContentToJSON :: [HeaderElement] -> String
headerContentToJSON [Title title] =
    replicate 8 ' ' ++ "\"title\": \"" ++ title ++ "\""
headerContentToJSON [Author author] =
    replicate 8 ' ' ++ "\"author\": \"" ++ author ++ "\""
headerContentToJSON [Date date] =
    replicate 8 ' ' ++ "\"date\": \"" ++ date ++ "\""
headerContentToJSON (Title title : xs) =
    replicate 8 ' ' ++ "\"title\": \"" ++ title ++ "\",\n"
    ++ headerContentToJSON xs
headerContentToJSON (Author author : xs) =
    replicate 8 ' ' ++ "\"author\": \"" ++ author ++ "\",\n"
    ++ headerContentToJSON xs
headerContentToJSON (Date date : xs) =
    replicate 8 ' ' ++ "\"date\": \"" ++ date ++ "\",\n"
    ++ headerContentToJSON xs
headerContentToJSON _ = ""

bodyToJSON :: Body -> String
bodyToJSON b = replicate 4 ' ' ++ "\"body\": [\n" ++
    listContentToJSON (content b) 1 ++ replicate 4 ' ' ++ "]"

listContentToJSON :: [Element] -> Int -> String
listContentToJSON [x] indent = contentToJSON x (indent + 1) True ++ "\n"
listContentToJSON (x:xs) indent = contentToJSON x (indent + 1) True ++
    ",\n" ++ listContentToJSON xs indent
listContentToJSON [] _ = ""

getIndentCount :: Int -> Bool -> Int
getIndentCount indent indentFirstLine =
    if indentFirstLine then indent * 4 else 0

contentToJSON :: Element -> Int -> Bool -> String
contentToJSON (Text text) indent indentFirstLine =
    replicate (getIndentCount indent indentFirstLine) ' '
    ++ "\"" ++ text ++ "\""
contentToJSON (List elements) indent indentFirstLine =
    replicate (getIndentCount indent indentFirstLine) ' '
    ++ listToJSON (List elements) indent
contentToJSON (CodeBlock elements) indent indentFirstLine =
    replicate (getIndentCount indent indentFirstLine) ' '
    ++ listToJSON (CodeBlock elements) indent
contentToJSON (Paragraph elements) indent _ =
    paragraphToJSON elements indent
contentToJSON (Link element) indent indentFirstLine =
    replicate (getIndentCount indent indentFirstLine) ' '
    ++ linksToJSON element indent
contentToJSON (Image element) indent indentFirstLine =
    replicate (getIndentCount indent indentFirstLine) ' '
    ++ imagesToJSON element indent
contentToJSON (Section sec) indent indentFirstLine =
    replicate (getIndentCount indent indentFirstLine) ' '
    ++ sectionToJSON sec indent
contentToJSON (Bold element) indent _ =
    formattedTextToJSON (Bold element) indent
contentToJSON (Italic element) indent _ =
    formattedTextToJSON (Italic element) indent
contentToJSON (Code element) indent _ =
    formattedTextToJSON (Code element) indent
contentToJSON Empty _ _ = ""

paragraphToJSON :: [Element] -> Int -> String
paragraphToJSON elements indent = replicate (indent * 4) ' '
    ++ "[\n" ++ listContentToJSON elements indent
    ++ replicate (indent * 4) ' ' ++ "]"

formattedTextToJSON :: Element -> Int -> String
formattedTextToJSON (Bold element) indent = replicate (indent * 4) ' ' ++
    "{\n" ++ replicate ((indent + 1) * 4) ' '
    ++ "\"bold\": " ++ contentToJSON element (indent + 1) False ++ "\n"
    ++ replicate (indent * 4) ' ' ++ "}"
formattedTextToJSON (Italic element) indent = replicate (indent * 4) ' ' ++
    "{\n" ++ replicate ((indent + 1) * 4) ' '
    ++ "\"italic\": " ++ contentToJSON element (indent + 1) False ++ "\n"
    ++ replicate (indent * 4) ' ' ++ "}"
formattedTextToJSON (Code element) indent = replicate (indent * 4) ' ' ++
    "{\n" ++ replicate ((indent + 1) * 4) ' '
    ++ "\"code\": " ++ contentToJSON element (indent + 1) False ++ "\n"
    ++ replicate (indent * 4) ' ' ++ "}"

listToJSON :: Element -> Int -> String
listToJSON (List elements) indent = "{\n" ++ replicate ((indent + 1) * 4) ' '
    ++ "\"list\": [\n" ++ listContentToJSON elements (indent + 1)
    ++ replicate ((indent + 1) * 4) ' ' ++ "]\n"
    ++ replicate (indent * 4) ' ' ++ "}"
listToJSON (CodeBlock elements) indent = "{\n" ++ replicate ((indent + 1) * 4) ' '
    ++ "\"codeblock\": [\n" ++ listContentToJSON elements (indent + 1)
    ++ replicate ((indent + 1) * 4) ' ' ++ "]\n"
    ++ replicate (indent * 4) ' ' ++ "}"
listToJSON _ _ = ""

linksToJSON :: LinkType -> Int -> String
linksToJSON link indent = "{\n" ++ replicate ((indent + 1) * 4) ' '
    ++ "\"link\": {\n" ++ replicate ((indent + 2) * 4) ' '
    ++ "\"url\": \"" ++ linkUrl link ++ "\",\n"
    ++ replicate ((indent + 2) * 4) ' '
    ++ "\"content\": [\n" ++ listContentToJSON (linkContent link) (indent + 2)
    ++ replicate ((indent + 2) * 4) ' '
    ++ "]\n" ++ replicate ((indent + 1) * 4) ' ' ++ "}\n"
    ++ replicate (indent * 4) ' ' ++ "}"

imagesToJSON :: ImageType -> Int -> String
imagesToJSON image indent = "{\n" ++ replicate ((indent + 1) * 4) ' '
    ++ "\"image\": {\n" ++ replicate ((indent + 2) * 4) ' '
    ++ "\"url\": \"" ++ imgUrl image ++ "\",\n"
    ++ replicate ((indent + 2) * 4) ' '
    ++ "\"alt\": [\n" ++ listContentToJSON (imgAlt image) (indent + 2)
    ++ replicate ((indent + 2) * 4) ' '
    ++ "]\n" ++ replicate ((indent + 1) * 4) ' ' ++ "}\n"
    ++ replicate (indent * 4) ' ' ++ "}"

sectionToJSON :: SectionType -> Int -> String
sectionToJSON section indent = "{\n" ++ replicate ((indent + 1) * 4) ' '
    ++ "\"section\": {\n" ++ replicate ((indent + 2) * 4) ' '
    ++ "\"title\": \"" ++ sectionTitle section ++ "\",\n"
    ++ replicate ((indent + 2) * 4) ' '
    ++ "\"content\": [\n"
    ++ listContentToJSON (sectionContent section) (indent + 2)
    ++ replicate ((indent + 2) * 4) ' '
    ++ "]\n" ++ replicate ((indent + 1) * 4) ' ' ++ "}\n"
    ++ replicate (indent * 4) ' ' ++ "}"
