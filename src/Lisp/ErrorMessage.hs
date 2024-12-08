{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- ErrorMessage
-}

module Lisp.ErrorMessage (errDef, errFnDef, errVarDef, errCall, errDefCtx, errIf, errLambda, errNonProcedure, errImpossible, errUnboundVar, errNumberArgs, errTypeArgs, errCond) where

errIf :: String -> String
errIf err = "Error: Invalid if: " ++ err

errCond :: String -> String
errCond err = "Error: Invalid cond: " ++ err

errLambda :: String -> String
errLambda err = "Error: Invalid lambda: " ++ err

errDefCtx :: String
errDefCtx = "Error: Invalid context for a define"

errCall :: String -> String
errCall err = "Error: Invalid call: " ++ err

errDef :: String
errDef = "Error: Invalid define"

errFnDef :: String -> String
errFnDef err = "Error: Invalid function definition: " ++ err

errVarDef :: String -> String
errVarDef err = "Error: Invalid variable definition: " ++ err

errUnboundVar :: String -> String
errUnboundVar name = "Error: variable " ++ name ++ " is not bound"

errImpossible :: String -> String
errImpossible err = "Impossible: This should never happend:" ++ err

errNonProcedure :: String -> String
errNonProcedure name = "Error: attempt to apply non-procedure " ++ name

errNumberArgs :: String -> String
errNumberArgs name = "Error: Invalid number of arguments in call: " ++ name

errTypeArgs :: String -> String -> String
errTypeArgs name t = "Error: Invalid type of arguments, expected " ++ t ++ ", in call: " ++ name
