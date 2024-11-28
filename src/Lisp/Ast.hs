{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- AST
-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Lisp.Ast
  ( sexprToAST,
    Ast (..),
  )
where

import Lisp.ErrorMessage
import Lisp.SExpression (SExpr (..), getSymbol)

-- TODO: The Body is a single expression, we don't handle many expression

data Ast
  = TInt Int
  | TSymbol String
  | TString String
  | TBool Bool
  | TVariable {varName :: String, varBody :: Ast}
  | TFunction {fnName :: String, fnArgs :: [String], defFnBody :: Ast}
  | TCall {fnName :: String, callFnBody :: Ast} -- TODO
  | TLambda -- TODO
  -- TVoid
  -- TList [Ast]
  deriving (Show)

type AstError = String

instance Eq Ast where
  -- (==) :: Ast -> Ast -> Bool
  TInt x == TInt y = x == y
  TSymbol x == TSymbol y = x == y
  TString x == TString y = x == y
  TBool x == TBool y = x == y
  -- TVoid == TVoid = True
  -- TList x == TList y = x == y
  TVariable name1 body1 == TVariable name2 body2 = name1 == name2 && body1 == body2
  TFunction fnName1 fnArgs1 fnBody1 == TFunction fnName2 fnArgs2 fnBody2 = fnName1 == fnName2 && fnArgs1 == fnArgs2 && fnBody1 == fnBody2
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

defineVariable :: String -> SExpr -> Either AstError Ast
defineVariable name value =
  case sexprToAST value of
    Right x | isDefine x -> Left $ errVarDef $ "cannot contain another define" ++ show x
    Right x -> Right $ TVariable name x
    Left err -> Left err

-- The body function cannot be [TVoid, TDefine, TVariable]
-- tho, it should never be able to contain a void since void can only be affected to symbols
defineFunction :: [SExpr] -> SExpr -> Either AstError Ast
defineFunction argsSExpr body =
  case mapM getSymbol argsSExpr of
    Just allArgs@(name : args)
      | not (any (`elem` keywords) allArgs) -> case sexprToAST body of
          Right x | isDefine x -> Left $ errFnDef $ "the body cannot contain a define:" ++ show x
          Right validBody -> Right $ TFunction name args validBody
          Left err -> Left $ errFnDef $ show err
      | otherwise -> Left $ errFnDef "variable's name cannot be Keywords"
    Just [] -> Left $ errFnDef "missing function name."
    Nothing -> Left $ errFnDef $ "variables' name must be Symbols." ++ show body

handleDefine :: [SExpr] -> Either AstError Ast
handleDefine [] = Left $ errDef "missing args and body."
handleDefine [_] = Left $ errDef "missing body."
handleDefine (SInt x : _) = Left $ errDef $ "args must be Symbols: " ++ show x
handleDefine (SString x : _) = Left $ errDef $ "args must be Symbols: " ++ show x
handleDefine [SList args, body] = defineFunction args body
handleDefine [SSymbol name, value] = defineVariable name value
handleDefine (_ : l : r) = Left $ errDef $ "the body must contain only one expression: " ++ show l ++ " " ++ show r

keywords :: [String]
keywords = ["define", "lambda"]

isDefine :: Ast -> Bool
isDefine (TFunction {}) = True
isDefine (TVariable {}) = True
isDefine _ = False
