{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- ErrorMessage
-}

module Lisp.ErrorMessage (errDef, errFnDef, errVarDef) where

errDef :: String -> String
errDef err = "Error: Invalid define: " ++ err

errFnDef :: String -> String
errFnDef err = "Error: Invalid function definition: " ++ err

errVarDef :: String -> String
errVarDef err = "Error: Invalid variable definition: " ++ err
