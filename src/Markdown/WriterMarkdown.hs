{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- WriterMarkdown
-}

module Markdown.WriterMarkdown (
        writeMarkdownDocument,
        headerToMarkdown,
        headerElementsToMarkdown,
        bodyToMarkdown,
        elementsToMarkdown,
        elementToMarkdown,
        listToMarkdown,
        paragraphToMarkdown,
        codeBlockToMarkdown,
        sectionTitleToMarkdown
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

writeMarkdownDocument :: Document -> String
writeMarkdownDocument (Document header body) =
    headerToMarkdown header ++ bodyToMarkdown body

headerToMarkdown :: Header -> String
headerToMarkdown (Header contents) =
    "---\n" ++ headerElementsToMarkdown contents ++ "---\n\n"

headerElementsToMarkdown :: [HeaderElement] -> String
headerElementsToMarkdown [] = ""
headerElementsToMarkdown ((Title title):xs) =
    "title: " ++ title ++ "\n" ++ headerElementsToMarkdown xs
headerElementsToMarkdown ((Author author):xs) =
    "author: " ++ author ++ "\n" ++ headerElementsToMarkdown xs
headerElementsToMarkdown ((Date date):xs) =
    "date: " ++ date ++ "\n" ++ headerElementsToMarkdown xs

bodyToMarkdown :: Body -> String
bodyToMarkdown (Body content) = elementsToMarkdown content 0 ++ "\n"

elementsToMarkdown :: [Element] -> Int -> String
elementsToMarkdown [] _ = ""
elementsToMarkdown [x] n = elementToMarkdown x n
elementsToMarkdown (x : xs) n = elementToMarkdown x n ++ "\n\n" ++
    elementsToMarkdown xs n

elementToMarkdown :: Element -> Int -> String
elementToMarkdown (Text str) _ = str
elementToMarkdown (Bold elem) n = "**" ++ elementToMarkdown elem n ++ "**"
elementToMarkdown (Italic elem) n = "*" ++ elementToMarkdown elem n ++ "*"
elementToMarkdown (Code elem) n = "`" ++ elementToMarkdown elem n ++ "`"
elementToMarkdown (List elems) n = listToMarkdown elems n
elementToMarkdown (Paragraph elems) n = paragraphToMarkdown elems n
elementToMarkdown (CodeBlock elems) n =
    "```\n" ++ codeBlockToMarkdown elems n ++ "```"
elementToMarkdown (Link (LinkType linkUrl linkContent)) n =
    "[" ++ paragraphToMarkdown linkContent n ++ "](" ++ linkUrl ++ ")"
elementToMarkdown (Image (ImageType imgUrl imgAlt)) n =
    "![" ++ paragraphToMarkdown imgAlt n ++ "](" ++ imgUrl ++ ")"
elementToMarkdown (Section (SectionType sectionTitle sectionContent)) n =
    sectionTitleToMarkdown sectionTitle (n + 1) ++
    elementsToMarkdown sectionContent (n + 1)
elementToMarkdown Empty _ = ""

listToMarkdown :: [Element] -> Int -> String
listToMarkdown [] _ = ""
listToMarkdown [x] n = "- " ++ elementToMarkdown x n
listToMarkdown (x : xs) n = "- " ++ elementToMarkdown x n ++ "\n" ++
    listToMarkdown xs n

paragraphToMarkdown :: [Element] -> Int -> String
paragraphToMarkdown [] _ = ""
paragraphToMarkdown (x : xs) n =
    elementToMarkdown x n ++ paragraphToMarkdown xs n

codeBlockToMarkdown :: [Element] -> Int -> String
codeBlockToMarkdown [] _ = ""
codeBlockToMarkdown (x : xs) n =
    elementToMarkdown x n ++ "\n" ++ codeBlockToMarkdown xs n

sectionTitleToMarkdown :: String -> Int -> String
sectionTitleToMarkdown "" _ = ""
sectionTitleToMarkdown title n = replicate n '#' ++ " " ++ title ++ "\n\n"
