{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- DataStruct
-}

module DataStruct (
        Document(..),
        Header(..),
        HeaderElement(..),
        Body(..),
        Element(..),
        LinkType(..),
        ImageType(..),
        SectionType(..)
    ) where

data Document = Document {
    header :: Header,
    body :: Body
    }

data Header = Header {
    contents :: [HeaderElement]
    }

data HeaderElement = Title String | Author String | Date String

data Body = Body {
    content :: [Element]
    }

data Element = Text String
    | Bold Element
    | Italic Element
    | Code Element
    | List [Element]
    | Paragraph [Element]
    | CodeBlock [Element]
    | Link LinkType
    | Image ImageType
    | Section SectionType

data LinkType = LinkType {
    linkUrl :: String,
    linkContent :: [Element]
    }

data ImageType = ImageType {
    imgUrl :: String,
    alt :: [Element]
    }

data SectionType = SectionType {
    sectionTitle :: String,
    sectionContent :: [Element]
    }
