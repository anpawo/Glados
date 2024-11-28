{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- SExpr
-}

module Lisp.SExpression (SExpr (..), getSymbol, getInteger, getList, printTree) where

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
printTree (SList x) = case mapM printTree x of
  Just l -> Just $ "List [" ++ show l ++ "]"
  _ -> Nothing
