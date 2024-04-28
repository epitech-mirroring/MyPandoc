{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- DataStructSpecs
-}

module DataStructSpecs (
        dataStructSpecs
    ) where

import Test.Hspec

import DataStruct (
        Document(..),
        Body(..),
        Header(..),
        HeaderElement(..),
        Element(..),
        LinkType(..),
        ImageType(..),
        SectionType(..)
    )

dataStructSpecs :: Spec
dataStructSpecs = describe "DataStruct" $ do
    documentSpecs
    headerSpecs
    headerElementSpecs
    bodySpecs
    elementSpecs
    linkTypeSpecs
    imageTypeSpecs
    sectionTypeSpecs

documentSpecs :: Spec
documentSpecs = describe "Document structure :" $ do
    it "should be equal" $ do
        let document = Document (Header []) (Body [])
        document `shouldBe` document
    it "should not be equal" $ do
        let document = Document (Header []) (Body [])
        let document' = Document (Header []) (Body [Text ""])
        document `shouldNotBe` document'
    it "accessors should work" $ do
        let document = Document (Header []) (Body [])
        header document `shouldBe` Header []
        body document `shouldBe` Body []
    it "show instance should work" $ do
        let document = Document (Header []) (Body [])
        show document `shouldBe` "Document {header = Header {contents = []}, body = Body {content = []}}"

headerSpecs :: Spec
headerSpecs = describe "Header structure :" $ do
    it "should be equal" $ do
        let header = Header []
        header `shouldBe` header
    it "should not be equal" $ do
        let header = Header []
        let header' = Header [Title ""]
        header `shouldNotBe` header'
    it "accessors should work" $ do
        let header = Header []
        contents header `shouldBe` []
    it "show instance should work" $ do
        let header = Header []
        show header `shouldBe` "Header {contents = []}"

headerElementSpecs :: Spec
headerElementSpecs = describe "HeaderElement structure :" $ do
    it "should be equal" $ do
        let headerElement = Title ""
        headerElement `shouldBe` headerElement
    it "should not be equal" $ do
        let headerElement = Title ""
        let headerElement' = Author ""
        headerElement `shouldNotBe` headerElement'
    it "accessors should work" $ do
        let headerElement = Title ""
        headerElement `shouldBe` Title ""
    it "show instance should work" $ do
        let headerElement = Title ""
        show headerElement `shouldBe` "Title \"\""

bodySpecs :: Spec
bodySpecs = describe "Body structure :" $ do
    it "should be equal" $ do
        let body = Body []
        body `shouldBe` body
    it "should not be equal" $ do
        let body = Body []
        let body' = Body [Text ""]
        body `shouldNotBe` body'
    it "accessors should work" $ do
        let body = Body []
        content body `shouldBe` []
    it "show instance should work" $ do
        let body = Body []
        show body `shouldBe` "Body {content = []}"

elementSpecs :: Spec
elementSpecs = describe "Element structure :" $ do
    it "should be equal" $ do
        let element = Text ""
        element `shouldBe` element
    it "should not be equal" $ do
        let element = Text ""
        let element' = Bold (Text "")
        element `shouldNotBe` element'
    it "accessors should work" $ do
        let element = Text ""
        element `shouldBe` Text ""
    it "show instance should work" $ do
        let element = Text ""
        show element `shouldBe` "Text \"\""

linkTypeSpecs :: Spec
linkTypeSpecs = describe "LinkType structure :" $ do
    it "should be equal" $ do
        let linkType = LinkType "" []
        linkType `shouldBe` linkType
    it "should not be equal" $ do
        let linkType = LinkType "" []
        let linkType' = LinkType "" [Text ""]
        linkType `shouldNotBe` linkType'
    it "accessors should work" $ do
        let linkType = LinkType "" []
        linkUrl linkType `shouldBe` ""
        linkContent linkType `shouldBe` []
    it "show instance should work" $ do
        let linkType = LinkType "" []
        show linkType `shouldBe` "LinkType {linkUrl = \"\", linkContent = []}"

imageTypeSpecs :: Spec
imageTypeSpecs = describe "ImageType structure :" $ do
    it "should be equal" $ do
        let imageType = ImageType "" []
        imageType `shouldBe` imageType
    it "should not be equal" $ do
        let imageType = ImageType "" []
        let imageType' = ImageType "" [Text ""]
        imageType `shouldNotBe` imageType'
    it "accessors should work" $ do
        let imageType = ImageType "" []
        imgUrl imageType `shouldBe` ""
        imgAlt imageType `shouldBe` []
    it "show instance should work" $ do
        let imageType = ImageType "" []
        show imageType `shouldBe` "ImageType {imgUrl = \"\", imgAlt = []}"

sectionTypeSpecs :: Spec
sectionTypeSpecs = describe "SectionType structure :" $ do
    it "should be equal" $ do
        let sectionType = SectionType "" []
        sectionType `shouldBe` sectionType
    it "should not be equal" $ do
        let sectionType = SectionType "" []
        let sectionType' = SectionType "" [Text ""]
        sectionType `shouldNotBe` sectionType'
    it "accessors should work" $ do
        let sectionType = SectionType "" []
        sectionTitle sectionType `shouldBe` ""
        sectionContent sectionType `shouldBe` []
    it "show instance should work" $ do
        let sectionType = SectionType "" []
        show sectionType `shouldBe` "SectionType {sectionTitle = \"\", sectionContent = []}"
