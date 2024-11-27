{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- SExpr
-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lisp.SExpression (SExpr (..)) where

import Data.Maybe (fromJust)

data SExpr
  = SInt Int
  | SSymbol String
  | SString String
  | SList [SExpr]
  deriving (Show)

getSymbol :: SExpr -> Maybe String
getSymbol (SSymbol x) = Just x
getSymbol _ = Nothing

getInteger :: SExpr -> Maybe Int
getInteger (SInt x) = Just x
getInteger _ = Nothing

getList :: SExpr -> Maybe [SExpr]
getList (SList x) = Just x
getList _ = Nothing

printTree :: SExpr -> Maybe String
printTree (SInt x) = Just ("Int " ++ show x)
printTree (SSymbol x) = Just ("Symbol " ++ x)
printTree (SString x) = Just ("String " ++ x)
printTree (SList x) = Just ("List [" ++ printRest x ++ "]")
  where
    printRest [SSymbol s] = "Symbol '" ++ s ++ "'"
    printRest [a] = fromJust (printTree a)
    printRest (SSymbol s : b) = "Symbol '" ++ s ++ "', " ++ printRest b
    printRest (a : b) = fromJust (printTree a) ++ ", " ++ printRest b
    printRest _ = ""
