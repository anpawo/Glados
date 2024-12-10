--
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Spec
--

import SExprParserSpec (spec)
import ErrorMessageSpec (spec)
import DisplaySpec (spec)
import AstSpec (spec)
import Test.Hspec (describe, hspec)

main :: IO ()
main = hspec $ do
  describe "Parsing String to SExpr" SExprParserSpec.spec
  describe "Check Error Messages" ErrorMessageSpec.spec
  describe "Check astToString and procedure" DisplaySpec.spec
  describe "Ast" AstSpec.spec
