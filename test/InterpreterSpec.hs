--
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- InterpreterSpec
--

{-# LANGUAGE OverloadedStrings #-}

module InterpreterSpec (spec) where

import Test.Hspec
import Lisp.Ast
import Lisp.Interpreter
import Lisp.Parser (parseInput, runParser)
import Lisp.SExpression (SExpr, printTree)
import System.IO (readFile)
import System.FilePath ((</>))
import Data.Text (pack)

spec :: Spec
spec = do
  describe "Interpreter" $ do
    it "interprets correctly" $ do
      input <- readFile ("test" </> "input" </> "input.csm")
      let parsed = runParser parseInput "" (pack input)
      case parsed of
        Left err -> expectationFailure $ "Parsing failed: " ++ show err
        Right sexpr -> do
          let ctx = [] :: Ctx
          result <- interpreter ctx sexpr
          output <- readFile ("test" </> "input" </> "expected_output.csm")
          case printTree result of
            Just resultStr -> resultStr `shouldBe` output
            Nothing -> expectationFailure "Failed to print SExpr"