{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- AST
-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Lisp.Ast
  ( sexprToAST,
    Ast (..),
  )
where

import Lisp.SExpression (SExpr (..))

data Ast
  = TInt Int
  | TSymbol String
  | TString String
  | TBool Bool
  | TVoid
  | -- above is 100% sure
    TVariable {varName :: String, varBody :: Ast} -- TODO: can be a value or a lambda
  | TDefine {fnName :: String, fnArgs :: [String], defFnBody :: [Ast]} -- TODO: body should be anything except another define
  | TCall {fnName :: String, callFnBody :: Ast} -- TODO
  | TLambda -- TODO
  | -- should exist ?
    TList [Ast]
  deriving (Show)

type AstError = String

instance Eq Ast where
  -- (==) :: Ast -> Ast -> Bool
  TInt x == TInt y = x == y
  TSymbol x == TSymbol y = x == y
  TString x == TString y = x == y
  TBool x == TBool y = x == y
  TVoid == TVoid = True
  TList x == TList y = x == y
  TVariable name1 body1 == TVariable name2 body2 = name1 == name2 && body1 == body2
  TDefine fnName1 fnArgs1 fnBody1 == TDefine fnName2 fnArgs2 fnBody2 = fnName1 == fnName2 && fnArgs1 == fnArgs2 && fnBody1 == fnBody2
  TCall fnName1 body1 == TCall fnName2 body2 = fnName1 == fnName2 && body1 == body2
  TLambda == TLambda = True
  _ == _ = False

sexprToAST :: SExpr -> Either AstError Ast
sexprToAST (SInt x) = Right (TInt x)
sexprToAST (SSymbol "#t") = Right (TBool True)
sexprToAST (SSymbol "#f") = Right (TBool False)
sexprToAST (SSymbol x) = Right (TSymbol x)
sexprToAST (SString x) = Right (TString x)
sexprToAST (SList x) = handleList x

handleList :: [SExpr] -> Either AstError Ast
handleList (SSymbol "define" : rst) = handleDefine rst
handleList functionNameAndArgs = handleCall functionNameAndArgs

handleCall :: [SExpr] -> Either AstError Ast
handleCall _ = Left "todo"

defineVariable :: String -> [SExpr] -> Either AstError Ast
defineVariable name [value] =
  case sexprToAST value of
    Right x -> Right (TVariable name x)
    Left err -> Left err
defineVariable _ _ = Left "defineVariable: not implemented yet (lambda)"

defineFunction :: [SExpr] -> [SExpr] -> Either AstError Ast
defineFunction args _ | not (all isSymbol args) = Left $ "Invalid function definition: variable's name must be Symbols." ++ show args
defineFunction args _ | any isKeyword args = Left $ "Invalid function definition: variable's name mustn't be Keywords." ++ show args
defineFunction ((SSymbol name) : args) body =
  case mapM sexprToAST body of
    Right l -> Right $ TDefine name (map (\(SSymbol s) -> s) args) l
    Left err -> Left err
defineFunction _ _ = Left "Invalid function definition: missing function name."

handleDefine :: [SExpr] -> Either AstError Ast
handleDefine [] = Left "Invalid define: missing function/variable name."
handleDefine (SInt x : _) = Left $ "Invalid define: cannot have a name starting with a number: " ++ show x
handleDefine (SString x : _) = Left $ "Invalid define: cannot have a string as name: " ++ show x
handleDefine (SSymbol name : value) = defineVariable name value
handleDefine (SList args : body) = defineFunction args body

isSymbol :: SExpr -> Bool
isSymbol (SSymbol _) = True
isSymbol _ = False

isKeyword :: SExpr -> Bool
isKeyword (SSymbol "define") = True
isKeyword (SSymbol "lambda") = True
isKeyword (SSymbol "set!") = True
isKeyword _ = False
