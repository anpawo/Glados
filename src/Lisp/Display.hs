{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Display
-}

module Lisp.Display (astToString) where

import Lisp.Ast (Ast (..))

procedure :: [String] -> String
procedure [name] = "#<procedure " ++ name ++ ">"
procedure _ = "#<procedure>"

astToString :: Ast -> String
astToString TVoid = "void"
astToString (TBool True) = "#t"
astToString (TBool False) = "#f"
astToString (TInt x) = show x
astToString (TFloat x) = show x
astToString (TString x) = show x
astToString (TLambda name _) = procedure name
astToString (TLambdaFly {}) = "lambdaFlyToString impossible"
astToString (TIf {}) = "ifToString impossible"
astToString (TCond {}) = "condToString impossible"
astToString (TFunction {}) = "functionToString impossible"
astToString (TVariable {}) = "variableToString impossible"
astToString (TDefineFunction {}) = "defineFunctionToString impossible"
astToString (TDefineVariable {}) = "defineVariableToString impossible"
astToString (TFunctionCall {}) = "variableCallToString impossible"
astToString (TVariableCall {}) = "functionCallToString impossible"
