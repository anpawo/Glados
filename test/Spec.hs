--
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Spec
--

-- import AstSpec (spec)
import SExprParserSpec (spec)
import ComputeSpec (spec)
import Test.Hspec (describe, hspec)

main :: IO ()
main = hspec $ do
  describe "Parsing String to SExpr" SExprParserSpec.spec
  describe "Compute the Ast code" ComputeSpec.spec

--   -- describe "Ast" AstSpec.spec
