--
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- EvaluateSpec
--

module EvaluateSpec (spec) where

import Test.Hspec
import Lisp.Evaluate
import Lisp.Ast
import Lisp.ErrorMessage

spec :: Spec
spec = do
  describe "evalAst" $ do
    it "evaluates TInt correctly" $ do
      evalAst [] (TInt 1) `shouldBe` Right (TInt 1, [])
    it "evaluates TFloat correctly" $ do
      evalAst [] (TFloat 1.0) `shouldBe` Right (TFloat 1.0, [])
    it "evaluates TBool correctly" $ do
      evalAst [] (TBool True) `shouldBe` Right (TBool True, [])
    it "evaluates TString correctly" $ do
      evalAst [] (TString "test") `shouldBe` Right (TString "test", [])
    it "evaluates TVoid correctly" $ do
      evalAst [] TVoid `shouldBe` Right (TVoid, [])
    it "evaluates TLambda correctly" $ do
      evalAst [] (TLambda [] TVoid) `shouldBe` Right (TLambda [] TVoid, [])
    it "evaluates TDefineVariable correctly" $ do
      evalAst [] (TDefineVariable "x" (TInt 1)) `shouldBe` Right (TVoid, [TVariable "x" (TInt 1)])
    it "evaluates TDefineFunction correctly" $ do
      evalAst [] (TDefineFunction "f" (TLambda [] TVoid)) `shouldBe` Right (TVoid, [TFunction "f" (TLambda [] TVoid)])
    it "evaluates TIf correctly" $ do
      evalAst [] (TIf (TBool True) (TInt 1) (TInt 2)) `shouldBe` Right (TInt 1, [])
      evalAst [] (TIf (TBool False) (TInt 1) (TInt 2)) `shouldBe` Right (TInt 2, [])
    it "evaluates TCond correctly" $ do
      evalAst [] (TCond [(TBool False, TInt 1), (TBool True, TInt 2)]) `shouldBe` Right (TInt 2, [])
    it "evaluates TVariableCall correctly" $ do
      evalAst [TVariable "x" (TInt 1)] (TVariableCall "x") `shouldBe` Right (TInt 1, [TVariable {varName = "x", varValue = TInt 1}])
    it "evaluates TFunctionCall correctly" $ do
      evalAst [TFunction "f" (TLambda ["x"] (TVariableCall "x"))] (TFunctionCall "f" [TInt 1]) `shouldBe` Right (TInt 1, [TFunction {fncName = "f", fncBody = TLambda {lambdaArgs = ["x"], lambdaBody = TVariableCall "x"}}])

  describe "builtin functions" $ do
    it "evaluates builtinEq correctly" $ do
      builtinEq [] [TInt 1, TInt 1] `shouldBe` Right (TBool True)
      builtinEq [] [TInt 1, TInt 2] `shouldBe` Right (TBool False)
      builtinEq [] [TFloat 1.0, TInt 1] `shouldBe` Right (TBool True)
      builtinEq [] [TFloat 1.0, TInt 2] `shouldBe` Right (TBool False)
    it "evaluates builtinZero correctly" $ do
      builtinZero [] [TInt 0] `shouldBe` Right (TBool True)
      builtinZero [] [TInt 1] `shouldBe` Right (TBool False)
      builtinZero [] [TFloat 0.0] `shouldBe` Right (TBool True)
      builtinZero [] [TFloat 1.0] `shouldBe` Right (TBool False)
    it "evaluates builtinAdd correctly" $ do
      builtinAdd [] [TInt 1, TInt 2] `shouldBe` Right (TInt 3)
      builtinAdd [] [TFloat 1.0, TFloat 2.0] `shouldBe` Right (TFloat 3.0)
      builtinAdd [] [TFloat 1.0, TInt 2] `shouldBe` Right (TFloat 3.0)
      builtinAdd [] [TInt 1, TFloat 2.0] `shouldBe` Right (TFloat 3.0)
    it "evaluates builtinSub correctly" $ do
      builtinSub [] [TInt 2, TInt 1] `shouldBe` Right (TInt 1)
      builtinSub [] [TFloat 2.0, TFloat 1.0] `shouldBe` Right (TFloat 1.0)
      builtinSub [] [TFloat 2.0, TInt 1] `shouldBe` Right (TFloat 1.0)
      builtinSub [] [TInt 2, TFloat 1.0] `shouldBe` Right (TFloat 1.0)
    it "evaluates builtinMul correctly" $ do
      builtinMul [] [TInt 2, TInt 3] `shouldBe` Right (TInt 6)
      builtinMul [] [TFloat 2.0, TFloat 3.0] `shouldBe` Right (TFloat 6.0)
      builtinMul [] [TFloat 2.0, TInt 3] `shouldBe` Right (TFloat 6.0)
      builtinMul [] [TInt 2, TFloat 3.0] `shouldBe` Right (TFloat 6.0)
    it "evaluates builtinDiv correctly" $ do
      builtinDiv [] [TInt 6, TInt 3] `shouldBe` Right (TInt 2)
      builtinDiv [] [TFloat 6.0, TFloat 3.0] `shouldBe` Right (TFloat 2.0)
      builtinDiv [] [TFloat 6.0, TInt 3] `shouldBe` Right (TFloat 2.0)
      builtinDiv [] [TInt 6, TFloat 3.0] `shouldBe` Right (TFloat 2.0)
    it "evaluates builtinMod correctly" $ do
      builtinMod [] [TInt 5, TInt 3] `shouldBe` Right (TInt 2)
      builtinMod [] [TFloat 5.0, TFloat 3.0] `shouldBe` Right (TFloat 2.0)
      builtinMod [] [TFloat 5.0, TInt 3] `shouldBe` Right (TFloat 2.0)
      builtinMod [] [TInt 5, TFloat 3.0] `shouldBe` Right (TFloat 2.0)
    it "evaluates builtinStringAppend correctly" $ do
      builtinStringAppend [] [TString "Hello", TString " World"] `shouldBe` Right (TString "Hello World")
      builtinStringAppend [] [TString "Foo", TString "Bar"] `shouldBe` Right (TString "FooBar")
    it "evaluates builtinNumberToString correctly" $ do
      builtinNumberToString [] [TInt 123] `shouldBe` Right (TString "123")
      builtinNumberToString [] [TFloat 123.45] `shouldBe` Right (TString "123.45")