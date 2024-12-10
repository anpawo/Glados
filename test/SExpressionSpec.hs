--
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- SExpressionSpec
--

module SExpressionSpec (spec) where

import Test.Hspec
import Lisp.SExpression

spec :: Spec
spec =  do
  describe "getSymbol" $ do
    it "returns the symbol when given an SSymbol" $ do
      getSymbol (SSymbol "test") `shouldBe` Right "test"
    it "returns an error when not given an SSymbol" $ do
      getSymbol (SInt 1) `shouldBe` Left "Expected Symbol"

  describe "getString" $ do
    it "returns the string when given an SString" $ do
      getString (SString "test") `shouldBe` Just "test"
    it "returns Nothing when not given an SString" $ do
      getString (SInt 1) `shouldBe` Nothing

  describe "getInteger" $ do
    it "returns the integer when given an SInt" $ do
      getInteger (SInt 1) `shouldBe` Just 1
    it "returns Nothing when not given an SInt" $ do
      getInteger (SSymbol "test") `shouldBe` Nothing

  describe "getFloat" $ do
    it "returns the float when given an SFloat" $ do
      getFloat (SFloat 1.0) `shouldBe` Just 1.0
    it "returns Nothing when not given an SFloat" $ do
      getFloat (SInt 1) `shouldBe` Nothing

  describe "getList" $ do
    it "returns the list when given an SList" $ do
      getList (SList [SInt 1, SFloat 2.0]) `shouldBe` Just [SInt 1, SFloat 2.0]
    it "returns Nothing when not given an SList" $ do
      getList (SInt 1) `shouldBe` Nothing

  describe "printTree" $ do
    it "returns the string representation of an SInt" $ do
      printTree (SInt 1) `shouldBe` Just "Int 1"
    it "returns the string representation of an SFloat" $ do
      printTree (SFloat 1.0) `shouldBe` Just "Float 1.0"
    it "returns the string representation of an SSymbol" $ do
      printTree (SSymbol "test") `shouldBe` Just "Symbol test"
    it "returns the string representation of an SString" $ do
      printTree (SString "test") `shouldBe` Just "String test"
    it "returns the string representation of an SList" $ do
      printTree (SList [SInt 1, SFloat 2.0]) `shouldBe` Just "List [[\"Int 1\",\"Float 2.0\"]]"
    --it "returns Nothing when the list contains an invalid SExpr" $ do
    --  printTree (SList []) `shouldBe` Nothing