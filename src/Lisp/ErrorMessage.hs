{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- ErrorMessage
-}

module Lisp.ErrorMessage (errDef, errFnDef, errVarDef, errCall, errDefCtx, errIf, errLambda) where

errIf :: String -> String
errIf err = "Error: Invalid if: " ++ err

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
