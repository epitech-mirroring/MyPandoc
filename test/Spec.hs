{-
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- Spec
-}

import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Test" $ do
        it "should be true" $ do
            True `shouldBe` False
