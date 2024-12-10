--
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Spec
--

import SExprParserSpec (spec)
import SExpressionSpec (spec)
--import InterpreterSpec (spec)
import EvaluateSpec (spec)
import ErrorMessageSpec (spec)
import DisplaySpec (spec)
import AstSpec (spec)
import Test.Hspec (describe, hspec)

main :: IO ()
main = hspec $ do
  describe "Parsing String to SExpr" SExprParserSpec.spec
  describe "SExpression getter" SExpressionSpec.spec
  --describe "Interpreter tests" InterpreterSpec.spec
  describe "Evaluate tests" EvaluateSpec.spec
--   -- describe "Ast" AstSpec.spec
  describe "Check Error Messages" ErrorMessageSpec.spec
  describe "Check astToString and procedure" DisplaySpec.spec
  describe "Ast" AstSpec.spec
