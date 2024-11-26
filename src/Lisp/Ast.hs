{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- AST
-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Lisp.Ast (sexprToAST) where

import Lisp.SExpression (SExpr (..))

data Ast
  = ADefine {aname :: String, abody :: Ast}
  | AInt Int
  | ASymbol String
  | ABool Bool
  | AList [Ast]
  deriving (Show)

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (SInt x) = Just (AInt x)
sexprToAST (SSymbol "#t") = Just (ABool True)
sexprToAST (SSymbol "#f") = Just (ABool False)
sexprToAST (SSymbol x) = Just (ASymbol x)
sexprToAST (SList (SSymbol "define" : SSymbol name : rst)) =
  Just (ADefine name (parseFunctionBody (SList rst)))
--   where
--     parseFunctionBody :: SExpr -> Ast
--     parseFunctionBody (SList [x]) = parseFunctionBody x
--     parseFunctionBody (SList (x : s)) = AList (parseFunctionBody x : [parseFunctionBody (SList s)])
--     parseFunctionBody (SList _) = AList (parseFunctionBody x : [parseFunctionBody (SList s)])
--     parseFunctionBody _ = AInt 0
sexprToAST _ = Nothing
