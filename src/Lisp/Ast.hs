{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- AST
-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lisp.Ast (sexprToAST) where

import Lisp.SExpression (SExpr (..))

data Ast
  = TDefine {fname :: String, args :: [Ast], body :: Ast}
  | TCall {fname :: String, body :: Ast}
  | TInt Int
  | TSymbol String
  | TBool Bool
  | TList [Ast]
  | TVoid -- empty define e.g: (define y)
  deriving (Show)

sexprToAST :: SExpr -> Either String Ast
sexprToAST (SInt x) = Right (TInt x)
sexprToAST (SSymbol "#t") = Right (TBool True)
sexprToAST (SSymbol "#f") = Right (TBool False)
sexprToAST (SSymbol x) = Right (TSymbol x)
sexprToAST (SList (SSymbol "define" : SInt _ : _)) = Left "Invalid define: cannot have a name starting with a number."
sexprToAST (SList (SSymbol "define" : SSymbol name : rst)) =
  case sexprToAST (SList rst) of
    Right x -> Right (TDefine name [] x)
    err -> err
sexprToAST (SList (SSymbol "define" : (SList x) : y)) =
  case sexprToAST (SList x) of
    Right x2 -> case sexprToAST (SList y) of
      Right (TList (TSymbol s : rst))
        | all isTSymbol rst -> Right (TDefine s rst x2)
        | otherwise -> Left "Invalid define: the arguments' name must be Strings."
        where
          isTSymbol :: Ast -> Bool
          isTSymbol (TSymbol _) = True
          isTSymbol _ = False
      Right _ -> Left "Invalid define: the name or arguments are invalid."
      err -> err
    err -> err
sexprToAST _ = Left "todo" -- Todo Call
-- sexprToAST (SList []) = Nothing

handleDefine :: [SExpr] -> Either String Ast
handleDefine (SInt x : _) = Left $ "Invalid define: cannot have a name starting with a number: " ++ show x
handleDefine (SList _ : _) = Left ""

-- handleCall :: [SExpr] -> Ast
