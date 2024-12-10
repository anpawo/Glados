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
      sexprToAST (SList [SSymbol "define", SSymbol "y", SInt 5]) `shouldBe` Right (TDefineVariable "y" (TInt 5))
    it "string into variable" $
      sexprToAST (SList [SSymbol "define", SSymbol "y", SString "Hello there"]) `shouldBe` Right (TDefineVariable "y" (TString "Hello there"))
    it "float into variable" $
      sexprToAST (SList [SSymbol "define", SSymbol "y", SFloat 3.14]) `shouldBe` Right (TDefineVariable "y" (TFloat 3.14))
    it "boolean true into variable" $
      sexprToAST (SList [SSymbol "define", SSymbol "y", SSymbol "#t"]) `shouldBe` Right (TDefineVariable "y" (TBool True))
    it "boolean false into variable" $
      sexprToAST (SList [SSymbol "define", SSymbol "y", SSymbol "#f"]) `shouldBe` Right (TDefineVariable "y" (TBool False))
    it "void into variable" $
      sexprToAST (SList [SSymbol "define", SSymbol "y"]) `shouldBe` Right (TDefineVariable "y" TVoid)
  
  describe "simple function definition" $ do
    it "no argument and default int value" $
      sexprToAST (SList [SSymbol "define", SList [SSymbol "int5"], SInt 5]) `shouldBe` Right (TDefineFunction "int5" (TLambda [] (TInt 5)))
    it "one argument and default int value" $
      sexprToAST (SList [SSymbol "define", SList [SSymbol "int5", SSymbol "unusedVar"], SInt 5]) `shouldBe` Right (TDefineFunction "int5" (TLambda ["unusedVar"] (TInt 5)))
    it "two arguments and default int value" $
      sexprToAST (SList [SSymbol "define", SList [SSymbol "int5", SSymbol "arg1", SSymbol "arg2"], SInt 5]) `shouldBe` Right (TDefineFunction "int5" (TLambda ["arg1", "arg2"] (TInt 5)))
    it "no argument and default string value" $
      sexprToAST (SList [SSymbol "define", SList [SSymbol "str"], SString "Hello"]) `shouldBe` Right (TDefineFunction "str" (TLambda [] (TString "Hello")))
    it "one argument and default string value" $
      sexprToAST (SList [SSymbol "define", SList [SSymbol "str", SSymbol "arg"], SString "Hello"]) `shouldBe` Right (TDefineFunction "str" (TLambda ["arg"] (TString "Hello")))

  describe "complex function definition" $ do
    it "nested function definition" $
      sexprToAST (SList [SSymbol "define", SList [SSymbol "outer", SSymbol "x"], SList [SSymbol "lambda", SList [SSymbol "y"], SList [SSymbol "+", SSymbol "x", SSymbol "y"]]]) `shouldBe` Right (TDefineFunction "outer" (TLambda ["x"] (TLambda ["y"] (TFunctionCall "+" [TVariableCall "x", TVariableCall "y"]))))
    it "function with if statement" $
      sexprToAST (SList [SSymbol "define", SList [SSymbol "check", SSymbol "x"], SList [SSymbol "if", SList [SSymbol ">", SSymbol "x", SInt 0], SString "Positive", SString "Non-positive"]]) `shouldBe` Right (TDefineFunction "check" (TLambda ["x"] (TIf (TFunctionCall ">" [TVariableCall "x", TInt 0]) (TString "Positive") (TString "Non-positive"))))

  describe "invalid if syntax" $ do
    it "missing all parts" $
      sexprToAST (SList [SSymbol "if"]) `shouldBe` Left "Error: Invalid if: expected (if cond then else)"
    it "missing else" $
      sexprToAST (SList [SSymbol "if", SSymbol "x", SInt 10]) `shouldBe` Right (TIf (TVariableCall "x") (TInt 10) TVoid)
    it "extra arguments" $
      sexprToAST (SList [SSymbol "if", SSymbol "x", SInt 10, SInt 20, SInt 30]) `shouldBe` Left "Error: Invalid if: expected (if cond then else)"

  describe "valid cond syntax" $ do
    it "else clause" $
      sexprToAST (SList [SSymbol "cond", SList [SSymbol "else", SInt 0]]) `shouldBe` Right (TCond [(TBool True, TInt 0)])

  describe "invalid cond syntax" $ do
    it "missing conditions and bodies" $
      sexprToAST (SList [SSymbol "cond"]) `shouldBe` Left "Error: Invalid cond: invalid syntax (you must provide at least one condition)"
    it "else not last" $
      sexprToAST (SList [SSymbol "cond", SList [SSymbol "else", SInt 0], SList [SSymbol "#t", SInt 1]]) `shouldBe` Left "Error: Invalid cond: else must be the last possible condition"
    it "non-list condition" $
      sexprToAST (SList [SSymbol "cond", SInt 5]) `shouldBe` Left "Error: Invalid cond: conditions must be lists"
