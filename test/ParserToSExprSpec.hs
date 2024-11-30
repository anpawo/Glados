--
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- ParserToSExprSpec
--

module ParserToSExprSpec (spec) where

import Data.Either (isLeft)
import qualified Lisp.Parser as LP
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec.Error (errorBundlePretty)

testParser :: LP.Parser LP.SExpr -> String -> Either String LP.SExpr
testParser p input =
  case LP.runParser p "" (LP.pack input) of
    Left err -> Left (errorBundlePretty err) -- doesnt work well
    Right x -> Right x

spec :: Spec
spec = do
  -- Int
  describe "parse Int success" $ do
    it "1" $
      testParser LP.parseSInt "1" `shouldBe` Right (LP.SInt 1)
    it "-12" $
      testParser LP.parseSInt "-12" `shouldBe` Right (LP.SInt (-12))
    it "+53a" $
      testParser LP.parseSInt "+53a" `shouldBe` Right (LP.SInt 53)
    it "-1a2" $
      testParser LP.parseSInt "-1a2" `shouldBe` Right (LP.SInt (-1))
    it "+53a6" $
      testParser LP.parseSInt "+53a6" `shouldBe` Right (LP.SInt 53)

  describe "parse Int failure" $ do
    it "a" $
      testParser LP.parseSInt "a" `shouldSatisfy` isLeft
    it "a1" $
      testParser LP.parseSInt "a1" `shouldSatisfy` isLeft
    it "-a1" $
      testParser LP.parseSInt "-a1" `shouldSatisfy` isLeft
    it "empty string" $
      testParser LP.parseSInt "" `shouldSatisfy` isLeft

  -- Symbol
  describe "parse Symbol success" $ do
    it "a" $
      testParser LP.parseSSymbol "a" `shouldBe` Right (LP.SSymbol "a")
    it "*" $
      testParser LP.parseSSymbol "*" `shouldBe` Right (LP.SSymbol "*")
    it "a2" $
      testParser LP.parseSSymbol "a2" `shouldBe` Right (LP.SSymbol "a2")
    it "*2+" $
      testParser LP.parseSSymbol "*2+" `shouldBe` Right (LP.SSymbol "*2+")

  describe "parse Symbol failure" $ do
    it "space" $
      testParser LP.parseSSymbol " " `shouldSatisfy` isLeft
    it "number first 1a" $
      testParser LP.parseSSymbol "1a" `shouldSatisfy` isLeft
    it "(" $
      testParser LP.parseSSymbol "(" `shouldSatisfy` isLeft
    it "empty string" $
      testParser LP.parseSSymbol "" `shouldSatisfy` isLeft

  -- String
  describe "parse String success" $ do
    it "\"hello\"" $
      testParser LP.parseSString "\"hello\"" `shouldBe` Right (LP.SString "hello")
    it "\"19 ans\"" $
      testParser LP.parseSString "\"19 ans\"" `shouldBe` Right (LP.SString "19 ans")
    it "\" 3 eme\"" $
      testParser LP.parseSString "\" 3 eme\"" `shouldBe` Right (LP.SString " 3 eme")
    it "\"empty string\"" $
      testParser LP.parseSString "\"\"" `shouldBe` Right (LP.SString "")

  describe "\"parse String failure\"" $ do
    it "\"" $
      testParser LP.parseSString "\"" `shouldSatisfy` isLeft
    it "\"number" $
      testParser LP.parseSString "\"number" `shouldSatisfy` isLeft
    it "number\"" $
      testParser LP.parseSString "number\"" `shouldSatisfy` isLeft

  -- List
  describe "parse List success" $ do
    it "(define (add x y) (+ x y))" $
      testParser LP.parseSList "(define (add x y) (+ x y))" `shouldBe` Right (LP.SList [LP.SSymbol "define", LP.SList [LP.SSymbol "add", LP.SSymbol "x", LP.SSymbol "y"], LP.SList [LP.SSymbol "+", LP.SSymbol "x", LP.SSymbol "y"]])
    it "weird spaces ( define(add x   y\t)(+ x\n y)   \r)" $
      testParser LP.parseSList "( define(add x   y\t)(+ x\n y)   \r)" `shouldBe` Right (LP.SList [LP.SSymbol "define", LP.SList [LP.SSymbol "add", LP.SSymbol "x", LP.SSymbol "y"], LP.SList [LP.SSymbol "+", LP.SSymbol "x", LP.SSymbol "y"]])
    it "(define (add* x y) (* (+ x y) 2))" $
      testParser LP.parseSList "(define (add* x y) (* (+ x y) 2))" `shouldBe` Right (LP.SList [LP.SSymbol "define", LP.SList [LP.SSymbol "add*", LP.SSymbol "x", LP.SSymbol "y"], LP.SList [LP.SSymbol "*", LP.SList [LP.SSymbol "+", LP.SSymbol "x", LP.SSymbol "y"], LP.SInt 2]])

-- need to finish this with the errors and more valid cases
