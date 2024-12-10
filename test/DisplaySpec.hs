--
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- DisplaySpec
--

module DisplaySpec (spec) where

import Test.Hspec
import Lisp.Display (astToString, procedure)
import Lisp.Ast (Ast (..))

spec :: Spec
spec = do
    describe "procedure" $ do
        it "returns a formatted procedure string with a name" $
            procedure ["myFunction"] `shouldBe` "#<procedure myFunction>"
        it "returns a generic procedure string when input is not a single name" $
            procedure [] `shouldBe` "#<procedure>"
        it "returns a generic procedure string when input has multiple names" $
            procedure ["myFunction", "extra"] `shouldBe` "#<procedure>"

    describe "astToString" $ do
        it "returns 'void' for TVoid" $
            astToString TVoid `shouldBe` "void"
        it "returns '#t' for TBool True" $
            astToString (TBool True) `shouldBe` "#t"
        it "returns '#f' for TBool False" $
            astToString (TBool False) `shouldBe` "#f"
        it "returns the string representation of an integer for TInt" $
            astToString (TInt 42) `shouldBe` "42"
        it "returns the string representation of a float for TFloat" $
            astToString (TFloat 3.14) `shouldBe` "3.14"
        it "returns the string representation of a string for TString" $
            astToString (TString "hello") `shouldBe` "\"hello\""
        it "returns a formatted procedure string for TLambda with a name" $
            astToString (TLambda ["myLambda"] TVoid) `shouldBe` "#<procedure myLambda>"
        it "returns 'lambdaFlyToString impossible' for TLambdaFly" $
            astToString (TLambdaFly [] TVoid) `shouldBe` "lambdaFlyToString impossible"
        it "returns 'ifToString impossible' for TIf" $
            astToString (TIf TVoid TVoid TVoid) `shouldBe` "ifToString impossible"
        it "returns 'condToString impossible' for TCond" $
            astToString (TCond [(TVoid, TVoid)]) `shouldBe` "condToString impossible"
        it "returns 'functionToString impossible' for TFunction" $
            astToString (TFunction "myFunction" TVoid) `shouldBe` "functionToString impossible"
        it "returns 'variableToString impossible' for TVariable" $
            astToString (TVariable "myVar" TVoid) `shouldBe` "variableToString impossible"
        it "returns 'defineFunctionToString impossible' for TDefineFunction" $
            astToString (TDefineFunction "myFunction" TVoid) `shouldBe` "defineFunctionToString impossible"
        it "returns 'defineVariableToString impossible' for TDefineVariable" $
            astToString (TDefineVariable "myVar" TVoid) `shouldBe` "defineVariableToString impossible"
        it "returns 'functionCallToString impossible' for TFunctionCall" $
            astToString (TFunctionCall "myFunction" [TVoid]) `shouldBe` "functionCallToString impossible"
        it "returns 'variableCallToString impossible' for TVariableCall" $
            astToString (TVariableCall "myVar") `shouldBe` "variableCallToString impossible"
