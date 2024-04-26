{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- Json writer in file
-}

module Json () where

import DataStruct (
    Document (header, body),
    Header (contents),
    Body (content),
    HeaderElement (Title, Author, Date),
    Element (..),
    ImageType (imgUrl, alt),
    SectionType (sectionTitle, sectionContent),
    LinkType (..)
    )

jsonWriter :: Document -> String
jsonWriter doc = "{\n" ++ headerWriter (header doc)
    ++ ",\n" ++ bodyWriter (body doc) ++ "\n}"

headerWriter :: Header -> String
headerWriter h = "    \"header\": {\n"
    ++ (headerContentWriter $ contents h) ++ "\n    }"

headerContentWriter :: [HeaderElement] -> String
headerContentWriter [x] = case x of
    Title title -> replicate 8 ' ' ++ "\"title\": \"" ++ title ++ "\""
    Author author -> replicate 8 ' ' ++ "\"author\": \"" ++ author ++ "\""
    Date date -> replicate 8 ' ' ++ "\"date\": \"" ++ date ++ "\""
headerContentWriter (x:xs) = case x of
    Title title -> replicate 8 ' ' ++ "\"title\": \"" ++ title ++ "\",\n"
        ++ headerContentWriter xs
    Author author -> replicate 8 ' ' ++ "\"author\": \"" ++ author ++ "\",\n"
        ++ headerContentWriter xs
    Date date -> replicate 8 ' ' ++ "\"date\": \"" ++ date ++ "\",\n"
        ++ headerContentWriter xs
headerContentWriter [] = ""

bodyWriter :: Body -> String
bodyWriter b = replicate 4 ' ' ++ "\"body\": [\n" ++
    listContentWriter (content b) 1 ++ "\n" ++ replicate 4 ' ' ++ "]"

listContentWriter :: [Element] -> Int -> String
listContentWriter [x] indent = contentWriter x (indent + 1) ++ "\n"
listContentWriter (x:xs) indent = contentWriter x (indent + 1) ++
    ",\n" ++ listContentWriter xs indent
listContentWriter [] _ = ""

contentWriter :: Element -> Int -> String
contentWriter x indent = case x of
    Text text -> replicate (indent * 4) ' ' ++ "\"" ++ text ++ "\""
    List _ -> replicate (indent * 4) ' ' ++ listWriter x indent
    CodeBlock _ -> replicate (indent * 4) ' ' ++ listWriter x indent
    Paragraph elements -> paragraphWriter elements indent
    Link element -> replicate (indent * 4) ' ' ++ linksWriter element indent
    Image element -> replicate (indent * 4) ' ' ++ imagesWriter element indent
    Section sec -> replicate (indent * 4) ' ' ++ sectionWriter sec indent
    _ -> formattedTextWriter x indent

paragraphWriter :: [Element] -> Int -> String
paragraphWriter elements indent = replicate (indent * 4) ' '
    ++ "[\n" ++ listContentWriter elements indent
    ++ replicate (indent * 4) ' ' ++ "]"

formattedTextWriter :: Element -> Int -> String
formattedTextWriter element indent = replicate (indent * 4) ' ' ++
    "{\n" ++ replicate ((indent + 1) * 4) ' '
    ++ "\"" ++ (case element of
        Bold _ -> "bold"
        Italic _ -> "italic"
        Code _ -> "code"
        _ -> "") ++ "\": " ++ contentWriter element (indent + 1) ++ "\n"
    ++ replicate (indent * 4) ' ' ++ "}"

listWriter :: Element -> Int -> String
listWriter element indent = "{\n" ++ replicate ((indent + 1) * 4) ' '
    ++ "\"" ++ (case element of
        List _ -> "list"
        CodeBlock _ -> "codeblock"
        _ -> "") ++ "\": [\n" ++ listContentWriter (case element of
        List elements -> elements
        CodeBlock elements -> elements
        _ -> []) (indent + 1) ++ replicate (indent * 4) ' ' ++ "]\n"
    ++ replicate (indent * 4) ' ' ++ "}"

linksWriter :: LinkType -> Int -> String
linksWriter link indent = "{\n" ++ replicate ((indent + 1) * 4) ' '
    ++ "\"link\": {\n" ++ replicate ((indent + 2) * 4) ' '
    ++ "\"url\": \"" ++ linkUrl link ++ "\",\n"
    ++ replicate ((indent + 2) * 4) ' '
    ++ "\"content\": [\n" ++ listContentWriter (linkContent link) (indent + 2)
    ++ replicate ((indent + 2) * 4) ' '
    ++ "]\n" ++ replicate ((indent + 1) * 4) ' ' ++ "}\n"
    ++ replicate (indent * 4) ' ' ++ "}"

imagesWriter :: ImageType -> Int -> String
imagesWriter image indent = "{\n" ++ replicate ((indent + 1) * 4) ' '
    ++ "\"image\": {\n" ++ replicate ((indent + 2) * 4) ' '
    ++ "\"url\": \"" ++ imgUrl image ++ "\",\n"
    ++ replicate ((indent + 2) * 4) ' '
    ++ "\"alt\": [\n" ++ listContentWriter (alt image) (indent + 2)
    ++ replicate ((indent + 2) * 4) ' '
    ++ "]\n" ++ replicate ((indent + 1) * 4) ' ' ++ "}\n"
    ++ replicate (indent * 4) ' ' ++ "}"

sectionWriter :: SectionType -> Int -> String
sectionWriter section indent = "{\n" ++ replicate ((indent + 1) * 4) ' '
    ++ "\"section\": {\n" ++ replicate ((indent + 2) * 4) ' '
    ++ "\"title\": \"" ++ sectionTitle section ++ "\",\n"
    ++ replicate ((indent + 2) * 4) ' '
    ++ "\"content\": [\n"
    ++ listContentWriter (sectionContent section) (indent + 2)
    ++ replicate ((indent + 2) * 4) ' '
    ++ "]\n" ++ replicate ((indent + 1) * 4) ' ' ++ "}\n"
    ++ replicate (indent * 4) ' ' ++ "}"
