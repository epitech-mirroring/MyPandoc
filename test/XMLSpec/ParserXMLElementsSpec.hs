{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- ParserXMLElementsSpec
-}

module XMLSpec.ParserXMLElementsSpec (
    parserXMLElementsSpec
) where

import XML.ParserXMLElements
import ParserData
import DataStruct
import Test.Hspec

parserXMLElementsSpec :: Spec
parserXMLElementsSpec = do
    parseXMLTextSpec
    parseXMLBoldSpec
    parseXMLItalicSpec
    parseXMLCodeSpec
    parseXMLListSpec
    parseXMLParagraphSpec
    parseXMLCodeBlockSpec
    parserXMLLinkSpec
    parseXMLImageSpec
    parseXMLSectionSpec
    parseXMLElementSpec

parseXMLTextSpec :: Spec
parseXMLTextSpec = describe "parseXMLText" $ do
    it "should parse a text" $ do
        runParser parseXMLText "text" `shouldBe` Just (Text "text", "")
    it "should parse a text" $ do
        runParser parseXMLText "text " `shouldBe` Just (Text "text ", "")
    it "should parse a text" $ do
        runParser parseXMLText "text</text>" `shouldBe` Just (Text "text", "</text>")
    it "should parse a text" $ do
        runParser parseXMLText "text  </text> " `shouldBe` Just (Text "text  ", "</text> ")
    it "should not parse a text" $ do
        runParser parseXMLText "" `shouldBe` Nothing
    it "should not parse a text" $ do
        runParser parseXMLText "<text>" `shouldBe` Nothing

parseXMLBoldSpec :: Spec
parseXMLBoldSpec = describe "parseXMLBold" $ do
    it "should parse a bold" $ do
        runParser parseXMLBold "<bold>text</bold>" `shouldBe` Just (Bold (Text "text"), "")
    it "should parse a bold" $ do
        runParser parseXMLBold "<bold>text</bold> " `shouldBe` Just (Bold (Text "text"), " ")
    it "should parse a bold" $ do
        runParser parseXMLBold "<bold>text</bold> a" `shouldBe` Just (Bold (Text "text"), " a")
    it "should not parse a bold" $ do
        runParser parseXMLBold "<bold>text</bold" `shouldBe` Nothing
    it "should not parse a bold" $ do
        runParser parseXMLBold "bold>text</bold>" `shouldBe` Nothing
    it "should parse a bold" $ do
        runParser parseXMLBold "<bold>text</bold> a" `shouldBe` Just (Bold (Text "text"), " a")
    it "should not parse a bold" $ do
        runParser parseXMLBold "a<bold>text</bold>" `shouldBe` Nothing
    it "should not parse a bold" $ do
        runParser parseXMLBold "<bald>text</bold>" `shouldBe` Nothing

parseXMLItalicSpec :: Spec
parseXMLItalicSpec = describe "parseXMLItalic" $ do
    it "should parse an italic" $ do
        runParser parseXMLItalic "<italic>text</italic>" `shouldBe` Just (Italic (Text "text"), "")
    it "should parse an italic" $ do
        runParser parseXMLItalic "<italic>text</italic> " `shouldBe` Just (Italic (Text "text"), " ")
    it "should parse an italic" $ do
        runParser parseXMLItalic "<italic>text</italic> a" `shouldBe` Just (Italic (Text "text"), " a")
    it "should not parse an italic" $ do
        runParser parseXMLItalic "<italic>text</italic" `shouldBe` Nothing
    it "should not parse an italic" $ do
        runParser parseXMLItalic "italic>text</italic>" `shouldBe` Nothing
    it "should parse an italic" $ do
        runParser parseXMLItalic "<italic>text</italic> a" `shouldBe` Just (Italic (Text "text"), " a")
    it "should not parse an italic" $ do
        runParser parseXMLItalic "a<italic>text</italic>" `shouldBe` Nothing
    it "should not parse an italic" $ do
        runParser parseXMLItalic "<italik>text</italic>" `shouldBe` Nothing

parseXMLCodeSpec :: Spec
parseXMLCodeSpec = describe "parseXMLCode" $ do
    it "should parse a code" $ do
        runParser parseXMLCode "<code>text</code>" `shouldBe` Just (Code (Text "text"), "")
    it "should parse a code" $ do
        runParser parseXMLCode "<code>text</code> " `shouldBe` Just (Code (Text "text"), " ")
    it "should parse a code" $ do
        runParser parseXMLCode "<code>text</code> a" `shouldBe` Just (Code (Text "text"), " a")
    it "should not parse a code" $ do
        runParser parseXMLCode "<code>text</code" `shouldBe` Nothing
    it "should not parse a code" $ do
        runParser parseXMLCode "code>text</code>" `shouldBe` Nothing
    it "should parse a code" $ do
        runParser parseXMLCode "<code>text</code> a" `shouldBe` Just (Code (Text "text"), " a")
    it "should not parse a code" $ do
        runParser parseXMLCode "a<code>text</code>" `shouldBe` Nothing
    it "should not parse a code" $ do
        runParser parseXMLCode "<coda>text</code>" `shouldBe` Nothing

parseXMLListSpec :: Spec
parseXMLListSpec = describe "parseXMLList" $ do
    it "should parse a list" $ do
        runParser parseXMLList "<list><bold>text</bold></list>" `shouldBe` Just (List [Bold (Text "text")], "")
    it "should parse a list" $ do
        runParser parseXMLList "<list><bold>text</bold></list> " `shouldBe` Just (List [Bold (Text "text")], "")
    it "should parse a list" $ do
        runParser parseXMLList "<list><bold>text</bold></list> a" `shouldBe` Just (List [Bold (Text "text")], "a")
    it "should not parse a list" $ do
        runParser parseXMLList "<list><bold>text</bold></list" `shouldBe` Nothing
    it "should not parse a list" $ do
        runParser parseXMLList "list><bold>text</bold></list>" `shouldBe` Nothing
    it "should parse a list" $ do
        runParser parseXMLList "<list><bold>text</bold></list> a" `shouldBe` Just (List [Bold (Text "text")], "a")
    it "should not parse a list" $ do
        runParser parseXMLList "a<list><bold>text</bold></list>" `shouldBe` Nothing
    it "should not parse a list" $ do
        runParser parseXMLList "<list><bald>text</bold></list>" `shouldBe` Nothing
    it "should parse a list" $ do
        runParser parseXMLList "<list><bold>text</bold><italic>text</italic></list>" `shouldBe` Just (List [Bold (Text "text"), Italic (Text "text")], "")
    it "should parse a list" $ do
        runParser parseXMLList "<list>lala <bold>text</bold> oh <italic>text</italic> hey.</list>" `shouldBe` Just (List [Text "lala ", Bold (Text "text"), Text " oh ", Italic (Text "text"), Text " hey."], "")

parseXMLParagraphSpec :: Spec
parseXMLParagraphSpec = describe "parseXMLParagraph" $ do
    it "should parse a paragraph" $ do
        runParser parseXMLParagraph "<paragraph><bold>text</bold></paragraph>" `shouldBe` Just (Paragraph [Bold (Text "text")], "")
    it "should parse a paragraph" $ do
        runParser parseXMLParagraph "<paragraph><bold>text</bold></paragraph> " `shouldBe` Just (Paragraph [Bold (Text "text")], "")
    it "should parse a paragraph" $ do
        runParser parseXMLParagraph "<paragraph><bold>text</bold></paragraph> a" `shouldBe` Just (Paragraph [Bold (Text "text")], "a")
    it "should not parse a paragraph" $ do
        runParser parseXMLParagraph "<paragraph><bold>text</bold></paragraph" `shouldBe` Nothing
    it "should not parse a paragraph" $ do
        runParser parseXMLParagraph "paragraph><bold>text</bold></paragraph>" `shouldBe` Nothing
    it "should parse a paragraph" $ do
        runParser parseXMLParagraph "<paragraph><bold>text</bold></paragraph> a" `shouldBe` Just (Paragraph [Bold (Text "text")], "a")
    it "should not parse a paragraph" $ do
        runParser parseXMLParagraph "a<paragraph><bold>text</bold></paragraph>" `shouldBe` Nothing
    it "should not parse a paragraph" $ do
        runParser parseXMLParagraph "<paragraph><bald>text</bold></paragraph>" `shouldBe` Nothing
    it "should parse a paragraph" $ do
        runParser parseXMLParagraph "<paragraph><bold>text</bold><italic>text</italic></paragraph>" `shouldBe` Just (Paragraph [Bold (Text "text"), Italic (Text "text")], "")
    it "should parse a paragraph" $ do
        runParser parseXMLParagraph "<paragraph>lala <bold>text</bold> oh <italic>text</italic> hey.</paragraph>" `shouldBe` Just (Paragraph [Text "lala ", Bold (Text "text"), Text " oh ", Italic (Text "text"), Text " hey."], "")

parseXMLCodeBlockSpec :: Spec
parseXMLCodeBlockSpec = describe "parseXMLCodeBlock" $ do
    it "should parse a codeblock" $ do
        runParser parseXMLCodeBlock "<codeblock><bold>text</bold></codeblock>" `shouldBe` Just (CodeBlock [Bold (Text "text")], "")
    it "should parse a codeblock" $ do
        runParser parseXMLCodeBlock "<codeblock><bold>text</bold></codeblock> " `shouldBe` Just (CodeBlock [Bold (Text "text")], "")
    it "should parse a codeblock" $ do
        runParser parseXMLCodeBlock "<codeblock><bold>text</bold></codeblock> a" `shouldBe` Just (CodeBlock [Bold (Text "text")], "a")
    it "should not parse a codeblock" $ do
        runParser parseXMLCodeBlock "<codeblock><bold>text</bold></codeblock" `shouldBe` Nothing
    it "should not parse a codeblock" $ do
        runParser parseXMLCodeBlock "codeblock><bold>text</bold></codeblock>" `shouldBe` Nothing
    it "should parse a codeblock" $ do
        runParser parseXMLCodeBlock "<codeblock><bold>text</bold></codeblock> a" `shouldBe` Just (CodeBlock [Bold (Text "text")], "a")
    it "should not parse a codeblock" $ do
        runParser parseXMLCodeBlock "a<codeblock><bold>text</bold></codeblock>" `shouldBe` Nothing
    it "should not parse a codeblock" $ do
        runParser parseXMLCodeBlock "<codeblock><bald>text</bold></codeblock>" `shouldBe` Nothing
    it "should parse a codeblock" $ do
        runParser parseXMLCodeBlock "<codeblock><bold>text</bold><italic>text</italic></codeblock>" `shouldBe` Just (CodeBlock [Bold (Text "text"), Italic (Text "text")], "")
    it "should parse a codeblock" $ do
        runParser parseXMLCodeBlock "<codeblock>lala <bold>text</bold> oh <italic>text</italic> hey.</codeblock>" `shouldBe` Just (CodeBlock [Text "lala ", Bold (Text "text"), Text " oh ", Italic (Text "text"), Text " hey."], "")

parserXMLLinkSpec :: Spec
parserXMLLinkSpec = describe "parseXMLLink" $ do
    it "should parse a link" $ do
        runParser parseXMLLink "<link url=\"url\"><bold>text</bold></link>" `shouldBe` Just (Link (LinkType "url" [Bold (Text "text")]), "")
    it "should parse a link" $ do
        runParser parseXMLLink "<link url=\"url\"><bold>text</bold></link> " `shouldBe` Just (Link (LinkType "url" [Bold (Text "text")]), "")
    it "should parse a link" $ do
        runParser parseXMLLink "<link url=\"url\"><bold>text</bold></link> a" `shouldBe` Just (Link (LinkType "url" [Bold (Text "text")]), "a")
    it "should not parse a link" $ do
        runParser parseXMLLink "<link url=\"url\"><bold>text</bold></link" `shouldBe` Nothing
    it "should not parse a link" $ do
        runParser parseXMLLink "link url=\"url\"><bold>text</bold></link>" `shouldBe` Nothing
    it "should parse a link" $ do
        runParser parseXMLLink "<link url=\"url\"><bold>text</bold></link> a" `shouldBe` Just (Link (LinkType "url" [Bold (Text "text")]), "a")
    it "should not parse a link" $ do
        runParser parseXMLLink "a<link url=\"url\"><bold>text</bold></link>" `shouldBe` Nothing
    it "should not parse a link" $ do
        runParser parseXMLLink "<link url=\"url\"><bald>text</bold></link>" `shouldBe` Nothing
    it "should parse a link" $ do
        runParser parseXMLLink "<link url=\"url\"><bold>text</bold><italic>text</italic></link>" `shouldBe` Just (Link (LinkType "url" [Bold (Text "text"), Italic (Text "text")]), "")
    it "should parse a link" $ do
        runParser parseXMLLink "<link url=\"url\">lala <bold>text</bold> oh <italic>text</italic> hey.</link>" `shouldBe` Just (Link (LinkType "url" [Text "lala ", Bold (Text "text"), Text " oh ", Italic (Text "text"), Text " hey."]), "")

parseXMLImageSpec :: Spec
parseXMLImageSpec = describe "parseXMLImage" $ do
    it "should parse an image" $ do
        runParser parseXMLImage "<image url=\"url\"><bold>text</bold></image>" `shouldBe` Just (Image (ImageType "url" [Bold (Text "text")]), "")
    it "should parse an image" $ do
        runParser parseXMLImage "<image url=\"url\"><bold>text</bold></image> " `shouldBe` Just (Image (ImageType "url" [Bold (Text "text")]), "")
    it "should parse an image" $ do
        runParser parseXMLImage "<image url=\"url\"><bold>text</bold></image> a" `shouldBe` Just (Image (ImageType "url" [Bold (Text "text")]), "a")
    it "should not parse an image" $ do
        runParser parseXMLImage "<image url=\"url\"><bold>text</bold></image" `shouldBe` Nothing
    it "should not parse an image" $ do
        runParser parseXMLImage "image url=\"url\"><bold>text</bold></image>" `shouldBe` Nothing
    it "should parse an image" $ do
        runParser parseXMLImage "<image url=\"url\"><bold>text</bold></image> a" `shouldBe` Just (Image (ImageType "url" [Bold (Text "text")]), "a")
    it "should not parse an image" $ do
        runParser parseXMLImage "a<image url=\"url\"><bold>text</bold></image>" `shouldBe` Nothing
    it "should not parse an image" $ do
        runParser parseXMLImage "<image url=\"url\"><bald>text</bold></image>" `shouldBe` Nothing
    it "should parse an image" $ do
        runParser parseXMLImage "<image url=\"url\"><bold>text</bold><italic>text</italic></image>" `shouldBe` Just (Image (ImageType "url" [Bold (Text "text"), Italic (Text "text")]), "")
    it "should parse an image" $ do
        runParser parseXMLImage "<image url=\"url\">lala <bold>text</bold> oh <italic>text</italic> hey.</image>" `shouldBe` Just (Image (ImageType "url" [Text "lala ", Bold (Text "text"), Text " oh ", Italic (Text "text"), Text " hey."]), "")

parseXMLSectionSpec :: Spec
parseXMLSectionSpec = describe "parseXMLSection" $ do
    it "should parse a section" $ do
        runParser parseXMLSection "<section title=\"title\"><bold>text</bold></section>" `shouldBe` Just (Section (SectionType "title" [Bold (Text "text")]), "")
    it "should parse a section" $ do
        runParser parseXMLSection "<section title=\"title\"><bold>text</bold></section> " `shouldBe` Just (Section (SectionType "title" [Bold (Text "text")]), "")
    it "should parse a section" $ do
        runParser parseXMLSection "<section title=\"title\"><bold>text</bold></section> a" `shouldBe` Just (Section (SectionType "title" [Bold (Text "text")]), "a")
    it "should not parse a section" $ do
        runParser parseXMLSection "<section title=\"title\"><bold>text</bold></section" `shouldBe` Nothing
    it "should not parse a section" $ do
        runParser parseXMLSection "section title=\"title\"><bold>text</bold></section>" `shouldBe` Nothing
    it "should parse a section" $ do
        runParser parseXMLSection "<section title=\"title\"><bold>text</bold></section> a" `shouldBe` Just (Section (SectionType "title" [Bold (Text "text")]), "a")
    it "should not parse a section" $ do
        runParser parseXMLSection "a<section title=\"title\"><bold>text</bold></section>" `shouldBe` Nothing
    it "should not parse a section" $ do
        runParser parseXMLSection "<section title=\"title\"><bald>text</bold></section>" `shouldBe` Nothing
    it "should parse a section" $ do
        runParser parseXMLSection "<section title=\"title\"><bold>text</bold><italic>text</italic></section>" `shouldBe` Just (Section (SectionType "title" [Bold (Text "text"), Italic (Text "text")]), "")
    it "should parse a section" $ do
        runParser parseXMLSection "<section title=\"title\">lala <bold>text</bold> oh <italic>text</italic> hey.</section>" `shouldBe` Just (Section (SectionType "title" [Text "lala ", Bold (Text "text"), Text " oh ", Italic (Text "text"), Text " hey."]), "")

parseXMLElementSpec :: Spec
parseXMLElementSpec = describe "parseXMLElement" $ do
    it "should parse an element" $ do
        runParser parseXMLElement "<bold>text</bold>" `shouldBe` Just (Bold (Text "text"), "")
    it "should parse an element" $ do
        runParser parseXMLElement "<italic>text</italic>" `shouldBe` Just (Italic (Text "text"), "")
    it "should parse an element" $ do
        runParser parseXMLElement "<code>text</code>" `shouldBe` Just (Code (Text "text"), "")
    it "should parse an element" $ do
        runParser parseXMLElement "<list><bold>text</bold></list>" `shouldBe` Just (List [Bold (Text "text")], "")
    it "should parse an element" $ do
        runParser parseXMLElement "<paragraph><bold>text</bold></paragraph>" `shouldBe` Just (Paragraph [Bold (Text "text")], "")
    it "should parse an element" $ do
        runParser parseXMLElement "<codeblock><bold>text</bold></codeblock>" `shouldBe` Just (CodeBlock [Bold (Text "text")], "")
    it "should parse an element" $ do
        runParser parseXMLElement "<link url=\"url\"><bold>text</bold></link>" `shouldBe` Just (Link (LinkType "url" [Bold (Text "text")]), "")
    it "should parse an element" $ do
        runParser parseXMLElement "<image url=\"url\"><bold>text</bold></image>" `shouldBe` Just (Image (ImageType "url" [Bold (Text "text")]), "")
    it "should parse an element" $ do
        runParser parseXMLElement "<section title=\"title\"><bold>text</bold></section>" `shouldBe` Just (Section (SectionType "title" [Bold (Text "text")]), "")
    it "should parse an element" $ do
        runParser parseXMLElement "text" `shouldBe` Just (Text "text", "")
    it "should not parse an element" $ do
        runParser parseXMLElement "" `shouldBe` Nothing
    it "should not parse an element" $ do
        runParser parseXMLElement "<text>" `shouldBe` Nothing
