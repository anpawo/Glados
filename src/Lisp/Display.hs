{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Display
-}

module Lisp.Display (astToString) where

import Lisp.Ast (Ast (..), Ctx)

procedure :: [String] -> String
procedure [name] = "#<procedure " ++ name ++ ">"
procedure _ = "#<procedure>"

astToString :: Ctx -> Ast -> String
astToString _ TVoid = "void"
astToString _ (TBool True) = "#t"
astToString _ (TBool False) = "#f"
astToString _ (TInt x) = show x
astToString _ (TString x) = show x
astToString _ (TLambda name _) = procedure name -- TODO: Lambda can be without any arguments
astToString _ _ = "todo: astToString (but should not happend)"
