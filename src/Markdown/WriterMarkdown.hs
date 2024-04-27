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
writeMarkdownDocument (Document header body) = reverse $
    removeEndWhiteSpaceMarkdown $
    reverse (headerToMarkdown header ++ bodyToMarkdown body)

removeEndWhiteSpaceMarkdown :: String -> String
removeEndWhiteSpaceMarkdown [] = []
removeEndWhiteSpaceMarkdown (x:xs)
    | x `elem` " \n\t" = removeEndWhiteSpaceMarkdown xs
    | otherwise = '\n' : xs

headerToMarkdown :: Header -> String
headerToMarkdown (Header contents) = "---\n" ++
    headerElementsToMarkdown contents ++ "---\n\n"

headerElementsToMarkdown :: [HeaderElement] -> String
headerElementsToMarkdown [] = ""
headerElementsToMarkdown ((Title title):xs) = "title: " ++ title ++ "\n" ++
    headerElementsToMarkdown xs
headerElementsToMarkdown ((Author author):xs) = "author: " ++ author ++ "\n" ++
    headerElementsToMarkdown xs
headerElementsToMarkdown ((Date date):xs) = "date: " ++ date ++ "\n" ++
    headerElementsToMarkdown xs

bodyToMarkdown :: Body -> String
bodyToMarkdown (Body content) = elementsToMarkdown content

elementsToMarkdown :: [Element] -> String
elementsToMarkdown [] = ""
elementsToMarkdown (x:xs) = elementToMarkdown x ++ "\n\n" ++
    elementsToMarkdown xs

elementToMarkdown :: Element -> String
elementToMarkdown (Text str) = str
elementToMarkdown (Bold elem) = "**" ++ elementToMarkdown elem ++ "**"
elementToMarkdown (Italic elem) = "*" ++ elementToMarkdown elem ++ "*"
elementToMarkdown (Code elem) = "`" ++ elementToMarkdown elem ++ "`"
elementToMarkdown (List elems) = listToMarkdown elems
elementToMarkdown (Paragraph elems) = paragraphToMarkdown elems
elementToMarkdown (CodeBlock elems) = "```\n" ++ codeBlockToMarkdown elems ++
    "```"
elementToMarkdown (Link link) = "[" ++
    paragraphToMarkdown (linkContent link) ++ "](" ++ linkUrl link ++ ")"
elementToMarkdown (Image img) = "!" ++ "[" ++
    paragraphToMarkdown (imgAlt img) ++ "](" ++ imgUrl img ++ ")"
elementToMarkdown (Section section) =
    sectionTitleToMarkdown (sectionTitle section) ++
    elementsToMarkdown (sectionContent section)
elementsToMarkdown Empty = ""

listToMarkdown :: [Element] -> String
listToMarkdown [] = ""
listToMarkdown [x] = "- " ++ elementToMarkdown x
listToMarkdown (x:xs) = "- " ++ elementToMarkdown x ++ "\n" ++
    listToMarkdown xs

paragraphToMarkdown :: [Element] -> String
paragraphToMarkdown [] = ""
paragraphToMarkdown (x:xs) = elementToMarkdown x ++ paragraphToMarkdown xs

codeBlockToMarkdown :: [Element] -> String
codeBlockToMarkdown [] = ""
codeBlockToMarkdown (x:xs) = elementToMarkdown x ++ "\n" ++
    codeBlockToMarkdown xs

sectionTitleToMarkdown :: String -> String
sectionTitleToMarkdown "" = "##"
sectionTitleToMarkdown title = "## " ++ title ++ "\n\n"
