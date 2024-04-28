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
        SectionType(..),
    ) where

data Document = Document {
    header :: Header,
    body :: Body
    } deriving (Show, Eq)

data Header = Header {
    contents :: [HeaderElement]
    } deriving (Show, Eq)

data HeaderElement = Title String | Author String | Date String
    deriving (Show, Eq)

data Body = Body {
    content :: [Element]
    } deriving (Show, Eq)

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
    | Empty
    deriving (Show, Eq)

data LinkType = LinkType {
    linkUrl :: String,
    linkContent :: [Element]
    } deriving (Show, Eq)

data ImageType = ImageType {
    imgUrl :: String,
    imgAlt :: [Element]
    } deriving (Show, Eq)

data SectionType = SectionType {
    sectionTitle :: String,
    sectionContent :: [Element]
    } deriving (Show, Eq)
