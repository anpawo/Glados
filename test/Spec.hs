--
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Spec
--

-- import AstSpec (spec)
import SExprParserSpec (spec)
import SExpressionSpec (spec)
--import InterpreterSpec (spec)
import EvaluateSpec (spec)
import Test.Hspec (describe, hspec)

main :: IO ()
main = hspec $ do
  describe "Parsing String to SExpr" SExprParserSpec.spec
  describe "SExpression getter" SExpressionSpec.spec
  --describe "Interpreter tests" InterpreterSpec.spec
  describe "Evaluate tests" EvaluateSpec.spec

--   -- describe "Ast" AstSpec.spec
