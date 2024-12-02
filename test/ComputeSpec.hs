--
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- TestComputeSpec
--

module ComputeSpec (spec) where

import Lisp.Ast (Ast (..), sexprToAST)
-- import Lisp.SExpression (SExpr (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Lisp.Compute (compute)

-- Test data for arithmetic operations
testAddition :: Ast
testAddition = TCall "+" [TInt 5, TInt 9]

testSubstraction :: Ast
testSubstraction = TCall "-" [TInt 10, TInt 4]

testMultiplication :: Ast
testMultiplication = TCall "*" [TInt 3, TInt 7]

testDivision :: Ast
testDivision = TCall "/" [TInt 20, TInt 4]

testDivisionByZero :: Ast
testDivisionByZero = TCall "/" [TInt 10, TInt 0]

--Test data for simple types
testInteger :: Ast
testInteger = TInt 10

testNegInteger :: Ast
testNegInteger = TInt (-10)


spec :: Spec
spec = do
  describe "Compute simple arithmetic operation" $ do
    it "computes addition" $
      compute testAddition `shouldBe` Right (TInt 14)

    it "computes subtraction" $
      compute testSubstraction `shouldBe` Right (TInt 6)

    it "computes multiplication" $
      compute testMultiplication `shouldBe` Right (TInt 21)

    it "computes division" $
      compute testDivision `shouldBe` Right (TInt 5)

    it "handles division by zero" $
      compute testDivisionByZero `shouldBe` Left "10 can't be divided by 0"

  describe "Compute simple types" $ do
    it "compute integer" $
      compute testInteger  `shouldBe` Right (TInt 10)

    it "compite neg integer" $
      compute testNegInteger `shouldBe` Right (TInt (-10))
