--
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- ParserToSExprSpec
--

module SExprParserSpec (spec) where

import Data.Either (isLeft, isRight)
import Data.Text (pack)
import qualified Lisp.Parser as LP
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec.Error (errorBundlePretty)

testParser :: LP.Parser LP.SExpr -> String -> Either String LP.SExpr
testParser p input =
  case LP.runParser p "" (pack input) of
    Left err -> Left (errorBundlePretty err)
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
    it "max Int" $
      testParser LP.parseSInt "2147483647" `shouldBe` Right (LP.SInt 2147483647)
    it "min Int" $
      testParser LP.parseSInt "-2147483648" `shouldBe` Right (LP.SInt (-2147483648))
    it "multiple leading zeros" $
      testParser LP.parseSInt "000123" `shouldBe` Right (LP.SInt 123)
    it "negative multiple leading zeros" $
      testParser LP.parseSInt "-000123" `shouldBe` Right (LP.SInt (-123))

  describe "parse Int failure" $ do
    it "a" $
      testParser LP.parseSInt "a" `shouldSatisfy` isLeft
    it "a1" $
      testParser LP.parseSInt "a1" `shouldSatisfy` isLeft
    it "-a1" $
      testParser LP.parseSInt "-a1" `shouldSatisfy` isLeft
    it "empty string" $
      testParser LP.parseSInt "" `shouldSatisfy` isLeft
    it "integer with whitespace" $
      testParser LP.parseSInt " 123 " `shouldSatisfy` isLeft

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
    it "symbol with special characters" $
      testParser LP.parseSSymbol "+-*/%&|^~!" `shouldBe` Right (LP.SSymbol "+-*/%&|^~!")

  describe "parse Symbol failure" $ do
    it "space" $
      testParser LP.parseSSymbol " " `shouldSatisfy` isLeft
    it "number first 1a" $
      testParser LP.parseSSymbol "1a" `shouldSatisfy` isLeft
    it "(" $
      testParser LP.parseSSymbol "(" `shouldSatisfy` isLeft
    it "empty string" $
      testParser LP.parseSSymbol "" `shouldSatisfy` isLeft
    it "symbol starting with number" $
      testParser LP.parseSSymbol "1abc" `shouldSatisfy` isLeft
    it "forbidden special characters in symbol" $
      testParser LP.parseSSymbol "()" `shouldSatisfy` isLeft
    it "symbol starting with number" $
      testParser LP.parseSSymbol "5_test" `shouldSatisfy` isLeft

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
    it "string with escaped quote" $
      testParser LP.parseSString "\"hello \\\"world\\\"\"" `shouldBe` Right (LP.SString "hello \\")
    it "string with newline escape" $
      testParser LP.parseSString "\"hello\\nworld\"" `shouldBe` Right (LP.SString "hello\\nworld")

  describe "\"parse String failure\"" $ do
    it "\"" $
      testParser LP.parseSString "\"" `shouldSatisfy` isLeft
    it "\"number" $
      testParser LP.parseSString "\"number" `shouldSatisfy` isLeft
    it "number\"" $
      testParser LP.parseSString "number\"" `shouldSatisfy` isLeft
    it "unescaped quote inside string" $
      testParser LP.parseSString "\"hello\"world\"" `shouldSatisfy` isRight
    it "unescaped quote inside list" $
      testParser LP.parseSList "(\"hello\"world\")" `shouldSatisfy` isLeft

  -- List
  describe "parse List success" $ do
    it "(define (add x y) (+ x y))" $
      testParser LP.parseSList "(define (add x y) (+ x y))" `shouldBe` Right (LP.SList [LP.SSymbol "define", LP.SList [LP.SSymbol "add", LP.SSymbol "x", LP.SSymbol "y"], LP.SList [LP.SSymbol "+", LP.SSymbol "x", LP.SSymbol "y"]])
    it "weird spaces ( define(add x   y\t)(+ x\n y)   \r)" $
      testParser LP.parseSList "( define(add x   y\t)(+ x\n y)   \r)" `shouldBe` Right (LP.SList [LP.SSymbol "define", LP.SList [LP.SSymbol "add", LP.SSymbol "x", LP.SSymbol "y"], LP.SList [LP.SSymbol "+", LP.SSymbol "x", LP.SSymbol "y"]])
    it "(define (add* x y) (* (+ x y) 2))" $
      testParser LP.parseSList "(define (add* x y) (* (+ x y) 2))" `shouldBe` Right (LP.SList [LP.SSymbol "define", LP.SList [LP.SSymbol "add*", LP.SSymbol "x", LP.SSymbol "y"], LP.SList [LP.SSymbol "*", LP.SList [LP.SSymbol "+", LP.SSymbol "x", LP.SSymbol "y"], LP.SInt 2]])
    it "deeply nested lists" $
      testParser LP.parseSList "(a (b (c (d (e)))))" `shouldBe` Right (LP.SList [LP.SSymbol "a", LP.SList [LP.SSymbol "b", LP.SList [LP.SSymbol "c", LP.SList [LP.SSymbol "d", LP.SList [LP.SSymbol "e"]]]]])
    it "list with leading spaces" $
      testParser LP.parseInput "  (a b c)" `shouldBe` Right (LP.SList [LP.SSymbol "a", LP.SSymbol "b", LP.SSymbol "c"])
    it "list with trailing spaces" $
      testParser LP.parseSList "(a b c)  " `shouldBe` Right (LP.SList [LP.SSymbol "a", LP.SSymbol "b", LP.SSymbol "c"])
    it "list with leading spaces" $
      testParser LP.parseSList "  (a b c)" `shouldSatisfy` isLeft
    it "unclosed list" $
      testParser LP.parseSList "(a b c" `shouldSatisfy` isLeft
    it "extra closing parenthesis" $
      testParser LP.parseInput "(a b c))" `shouldSatisfy` isLeft
    it "mixed list with ints, symbols, and strings" $
      testParser LP.parseSList "(123 \"hello\" world -456)" `shouldBe` Right (LP.SList [LP.SInt 123, LP.SString "hello", LP.SSymbol "world", LP.SInt (-456)])
    it "list with invalid types" $
      testParser LP.parseSList "(123 \"unclosed string  world -456)" `shouldSatisfy` isLeft

  -- Input
  describe "parse good Input" $ do
    it "complex S-expression input" $
      testParser LP.parseInput "(define (square x) (* x x))" `shouldBe` Right (LP.SList [LP.SSymbol "define", LP.SList [LP.SSymbol "square", LP.SSymbol "x"], LP.SList [LP.SSymbol "*", LP.SSymbol "x", LP.SSymbol "x"]])
 
  describe "parse bad Input" $ do
    it "input with invalid S-expression parts" $
      testParser LP.parseInput "(define 123abc)" `shouldSatisfy` isLeft
