--
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- TestTemplateSpec
--

module AstSpec (spec) where

import Lisp.Ast (Ast (..), sexprToAST)
import Lisp.SExpression (SExpr (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "simple variable definition" $ do
    it "int into variable" $
      sexprToAST (SList [SSymbol "define", SSymbol "y", SInt 5]) `shouldBe` Right (TVariable "y" (TInt 5))
    it "string into variable" $
      sexprToAST (SList [SSymbol "define", SSymbol "y", SString "Hello there"]) `shouldBe` Right (TVariable "y" (TString "Hello there"))

  describe "simple function definition" $ do
    it "no argument and default int value" $
      sexprToAST (SList [SSymbol "define", SList [SSymbol "int5"], SInt 5]) `shouldBe` Right (TFunction "int5" [] (TInt 5))
    it "one argument and default int value" $
      sexprToAST (SList [SSymbol "define", SList [SSymbol "int5", SSymbol "unusedVar"], SInt 5]) `shouldBe` Right (TFunction "int5" ["unusedVar"] (TInt 5))
