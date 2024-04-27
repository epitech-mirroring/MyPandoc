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
headerContentToJSON [x] = case x of
    Title title -> replicate 8 ' ' ++ "\"title\": \"" ++ title ++ "\""
    Author author -> replicate 8 ' ' ++ "\"author\": \"" ++ author ++ "\""
    Date date -> replicate 8 ' ' ++ "\"date\": \"" ++ date ++ "\""
headerContentToJSON (x:xs) = case x of
    Title title -> replicate 8 ' ' ++ "\"title\": \"" ++ title ++ "\",\n"
        ++ headerContentToJSON xs
    Author author -> replicate 8 ' ' ++ "\"author\": \"" ++ author ++ "\",\n"
        ++ headerContentToJSON xs
    Date date -> replicate 8 ' ' ++ "\"date\": \"" ++ date ++ "\",\n"
        ++ headerContentToJSON xs
headerContentToJSON [] = ""

bodyToJSON :: Body -> String
bodyToJSON b = replicate 4 ' ' ++ "\"body\": [\n" ++
    listContentToJSON (content b) 1 ++ replicate 4 ' ' ++ "]"

listContentToJSON :: [Element] -> Int -> String
listContentToJSON [x] indent = contentToJSON x (indent + 1) ++ "\n"
listContentToJSON (x:xs) indent = contentToJSON x (indent + 1) ++
    ",\n" ++ listContentToJSON xs indent
listContentToJSON [] _ = ""

contentToJSON :: Element -> Int -> String
contentToJSON (Text text) indent =
    replicate (indent * 4) ' ' ++ "\"" ++ text ++ "\""
contentToJSON (List elements) indent =
    replicate (indent * 4) ' ' ++ listToJSON (List elements) indent
contentToJSON (CodeBlock elements) indent =
    replicate (indent * 4) ' ' ++ listToJSON (CodeBlock elements) indent
contentToJSON (Paragraph elements) indent =
    replicate (indent * 4) ' ' ++ paragraphToJSON elements indent
contentToJSON (Link element) indent =
    replicate (indent * 4) ' ' ++ linksToJSON element indent
contentToJSON (Image element) indent =
    replicate (indent * 4) ' ' ++ imagesToJSON element indent
contentToJSON (Section sec) indent =
    replicate (indent * 4) ' ' ++ sectionToJSON sec indent
contentToJSON (Bold element) indent =
    formattedTextToJSON (Bold element) indent
contentToJSON (Italic element) indent =
    formattedTextToJSON (Italic element) indent
contentToJSON (Code element) indent =
    formattedTextToJSON (Code element) indent
contentToJSON Empty indent = ""
--contentToJSON x indent = case x of
--   Text text -> replicate (indent * 4) ' ' ++ "\"" ++ text ++ "\""
--   List _ -> replicate (indent * 4) ' ' ++ listToJSON x indent
--   CodeBlock _ -> replicate (indent * 4) ' ' ++ listToJSON x indent
--   Paragraph elements -> paragraphToJSON elements indent
--   Link element -> replicate (indent * 4) ' ' ++ linksToJSON element indent
--   Image element -> replicate (indent * 4) ' ' ++ imagesToJSON element indent
--   Section sec -> replicate (indent * 4) ' ' ++ sectionToJSON sec indent
--   Empty -> ""
--   _ -> formattedTextToJSON x indent

paragraphToJSON :: [Element] -> Int -> String
paragraphToJSON elements indent = replicate (indent * 4) ' '
    ++ "[\n" ++ listContentToJSON elements indent
    ++ replicate (indent * 4) ' ' ++ "]"

formattedTextToJSON :: Element -> Int -> String
formattedTextToJSON (Bold element) indent = replicate (indent * 4) ' ' ++
    "{\n" ++ replicate ((indent + 1) * 4) ' '
    ++ "\"bold\": " ++ contentToJSON element (indent + 1) ++ "\n"
    ++ replicate (indent * 4) ' ' ++ "}"
formattedTextToJSON (Italic element) indent = replicate (indent * 4) ' ' ++
    "{\n" ++ replicate ((indent + 1) * 4) ' '
    ++ "\"italic\": " ++ contentToJSON element (indent + 1) ++ "\n"
    ++ replicate (indent * 4) ' ' ++ "}"
formattedTextToJSON (Code element) indent = replicate (indent * 4) ' ' ++
    "{\n" ++ replicate ((indent + 1) * 4) ' '
    ++ "\"code\": " ++ contentToJSON element (indent + 1) ++ "\n"
    ++ replicate (indent * 4) ' ' ++ "}"
-- formattedTextToJSON element indent = replicate (indent * 4) ' ' ++
--     "{\n" ++ replicate ((indent + 1) * 4) ' '
--     ++ "\"" ++ (case element of
--         Bold _ -> "bold"
--         Italic _ -> "italic"
--         Code _ -> "code"
--         _ -> "") ++ "\": " ++ contentToJSON element (indent + 1) ++ "\n"
--     ++ replicate (indent * 4) ' ' ++ "}"

listToJSON :: Element -> Int -> String
listToJSON element indent = "{\n" ++ replicate ((indent + 1) * 4) ' '
    ++ "\"" ++ (case element of
        List _ -> "list"
        CodeBlock _ -> "codeblock"
        _ -> "") ++ "\": [\n" ++ listContentToJSON (case element of
        List elements -> elements
        CodeBlock elements -> elements
        _ -> []) (indent + 1) ++ replicate (indent * 4) ' ' ++ "]\n"
    ++ replicate (indent * 4) ' ' ++ "}"

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
