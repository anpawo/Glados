--
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- ErrorMessageSpec
--

module ErrorMessageSpec (spec) where

import Test.Hspec
import Lisp.ErrorMessage

spec :: Spec
spec = do
    describe "ErrorMessage" $ do
        it "generates an error for invalid 'if' conditions" $
            errIf "condition failed" `shouldBe` "Error: Invalid if: condition failed"
        it "generates an error for invalid 'cond' expressions" $
            errCond "condition failed" `shouldBe` "Error: Invalid cond: condition failed"
        it "generates an error for invalid 'lambda' expressions" $
            errLambda "lambda failed" `shouldBe` "Error: Invalid lambda: lambda failed"
        it "generates an error for invalid 'define' context" $
            errDefCtx `shouldBe` "Error: Invalid context for a define"
        it "generates an error for invalid 'function calls'" $
            errCall "call failed" `shouldBe` "Error: Invalid call: call failed"
        it "generates an error for invalid 'definitions'" $
            errDef `shouldBe` "Error: Invalid define"
        it "generates an error for invalid 'function' definitions" $
            errFnDef "function definition failed" `shouldBe` "Error: Invalid function definition: function definition failed"
        it "generates an error for invalid 'variable' definitions" $
            errVarDef "variable definition failed" `shouldBe` "Error: Invalid variable definition: variable definition failed"
        it "generates an error for unbound 'variables'" $
            errUnboundVar "x" `shouldBe` "Error: variable x is not bound"
        it "generates an error for 'impossible' cases" $
            errImpossible "unexpected error" `shouldBe` "Impossible: This should never happend:unexpected error"
        it "generates an error for applying 'non-procedures'" $
            errNonProcedure "x" `shouldBe` "Error: attempt to apply non-procedure x"
        it "generates an error for invalid 'argument counts'" $
            errNumberArgs "function call" `shouldBe` "Error: Invalid number of arguments in call: function call"
        it "generates an error for invalid 'argument types'" $
            errTypeArgs "function call" "int" `shouldBe` "Error: Invalid type of arguments, expected int, in call: function call"
