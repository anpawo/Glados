{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Lib
-}

module Lib
  ( someFunc,
  )
where

import Data.List
import Data.Maybe
import Text.Read

data SExpr = Sint Int | Sstring String | Slist [SExpr] deriving (Show)

data Ast = Define String Ast
         | Sum Ast Ast
         | Minus Ast Ast
         | Var String
         | Let String Ast Ast
         | Aint Int
         deriving (Show)

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (exp)
    | Sstring s <- exp = sstringToAST exp
    | Sint i <- exp = Just (Aint i)
    | Sstring s <- exp = Just (Var s)

sstringToAST :: SExpr -> Maybe Ast
sstringToAST (exp)
    | Sstring ('d':'e':'f':'i':'n':'e':' ':s) <- exp = Just (Define s (fromJust (sexprToAST (Sstring s))))
    | Sstring s <- exp = Just (Var s)
    | otherwise = Nothing


getSymbol :: SExpr -> Maybe String
getSymbol (exp)
    | Sstring s <- exp = Just s
    | otherwise = Nothing

getNumber :: SExpr -> Maybe Int
getNumber (exp)
    | Sint i <- exp = Just i
    | otherwise = Nothing

getSExpr :: SExpr -> Maybe [SExpr]
getSExpr (exp)
    | Slist l <- exp = Just l
    | otherwise = Nothing

printTree :: SExpr -> Maybe String
printTree (exp)
    | Sint i <- exp = printSint exp
    | Sstring s <- exp = printSstring exp
    | Slist l <- exp = printSlist exp

printSint :: SExpr -> Maybe String
printSint (Sint i) = Just ("a Number " ++ show i)

printSstring :: SExpr -> Maybe String
printSstring (Sstring s) = Just ("a Symbol " ++ s)

printSlist :: SExpr -> Maybe String
printSlist exp
    | Slist l <- exp = Just ("a List with " ++ concatMap (\e -> fromJust (printTree e) ++ " followed by ") (init l) ++ fromJust (printTree (last l)))

someFunc :: IO ()
someFunc = putStrLn "templateFunction"
