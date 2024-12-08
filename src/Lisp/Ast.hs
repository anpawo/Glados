{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- AST
-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Lisp.Ast
  ( sexprToAST,
    Ast (..),
    Ctx,
    catchDefine,
  )
where

import Data.List (singleton)
import Lisp.ErrorMessage
import Lisp.SExpression (SExpr (..), getSymbol)

data Ast
  = -- basic types && consumed by execute to display
    TInt Int
  | TFloat Float
  | TBool Bool
  | TVoid
  | TString String
  | TIf {ifCond :: Ast, ifThen :: Ast, ifElse :: Ast}
  | TCond {condBody :: [(Ast, Ast)]} -- [Cond:Body, Cond:Body, else: Body]
  | TLambdaFly {givenArgs :: [Ast], lBody :: Ast} -- Lambda body
  | -- lambda type (functions{args, body})
    TLambda {lambdaArgs :: [String], lambdaBody :: Ast}
  | -- what will be kept inside the ctx
    TFunction {fncName :: String, fncBody :: Ast} -- value is lambda
  | TVariable {varName :: String, varValue :: Ast} -- value is basic type (any function call or if should be executed during the define)
  | -- consumed by execute to add some ctx
    TDefineFunction {defFncName :: String, defFncBody :: Ast} -- Lambda or If
  | TDefineVariable {defVarName :: String, defVarBody :: Ast} -- Basic Type
  | -- consumed by execute to display
    TFunctionCall {callName :: String, callArgs :: [Ast]}
  | TVariableCall String
  deriving (Eq, Show)

type AstError = String

type Ctx = [Ast]

-- TODO: prevent edge cases like (define x (define y))
sexprToAST :: SExpr -> Either AstError Ast
sexprToAST (SInt x) = Right $ TInt x
sexprToAST (SFloat x) = Right $ TFloat x
sexprToAST (SSymbol "#t") = Right $ TBool True
sexprToAST (SSymbol "#f") = Right $ TBool False
sexprToAST (SSymbol x) = Right $ TVariableCall x
sexprToAST (SString x) = Right $ TString x
sexprToAST (SList x) = handleList x

handleList :: [SExpr] -> Either AstError Ast
handleList (SSymbol "define" : rst) = handleDefine rst
handleList (SSymbol "if" : rst) = handleIf rst
handleList (SSymbol "lambda" : rst) = handleLambda rst
handleList (SSymbol "cond" : rst) = handleCond rst
handleList functionNameAndArgs = handleCall functionNameAndArgs

-- Cond
handleCond :: [SExpr] -> Either AstError Ast
handleCond [] = Left $ errCond "invalid syntax (you must provide at least one condition)"
handleCond x = consumeCond x >>= Right . TCond
  where
    consumeCond :: [SExpr] -> Either AstError [(Ast, Ast)]
    consumeCond [] = Right [(TBool True, TVoid)]
    consumeCond [SList [SSymbol "else", body]] = ((,) <$> Right (TBool True) <*> sexprToAST body) >>= Right . singleton -- Right [(TBool True, sexprToAST body)]
    consumeCond (SList [SSymbol "else", _] : _) = Left $ errCond "else must be the last possible condition"
    consumeCond (SList [cond, body] : rst) = (:) <$> ((,) <$> sexprToAST cond <*> sexprToAST body) <*> consumeCond rst
    consumeCond (SList [condAndBody] : rst) = (:) <$> ((,) <$> sexprToAST condAndBody <*> sexprToAST condAndBody) <*> consumeCond rst
    consumeCond _ = Left $ errCond "conditions must be lists"

-- Cond

-- Lambda
handleLambda :: [SExpr] -> Either AstError Ast
handleLambda [] = Left $ errLambda "missing arguments and body"
handleLambda [_] = Left $ errLambda "missing body"
handleLambda [SList args, body] = TLambda <$> mapM getSymbol args <*> sexprToAST body
handleLambda _ = Left $ errLambda "expected (lambda (args) body)"

-- Lambda

-- If
handleIf :: [SExpr] -> Either AstError Ast
handleIf [a, b, c] = TIf <$> sexprToAST a <*> sexprToAST b <*> sexprToAST c
handleIf [a, b] = TIf <$> sexprToAST a <*> sexprToAST b <*> Right TVoid
handleIf _ = Left $ errIf "expected (if cond then else)"

-- If

-- Call
--    the whole call process is in the execution of the statement
--    not during its creation
handleCall :: [SExpr] -> Either AstError Ast
handleCall [] = Left $ errCall "missing function name"
handleCall (x@(SList _) : args) = (TLambdaFly <$> mapM sexprToAST args <*> sexprToAST x) >>= Right
handleCall (name : args) = (TFunctionCall <$> getSymbol name <*> mapM sexprToAST args) >>= Right

-- Call

-- Define
handleDefine :: [SExpr] -> Either AstError Ast
handleDefine [] = Left $ errVarDef "missing variable name"
-- Void Variable Definition
handleDefine [SSymbol name] = Right $ TDefineVariable name TVoid
-- Function Definition
handleDefine [SSymbol name, SList [SSymbol "lambda", SList args, body]] = TDefineFunction <$> Right name <*> (TLambda <$> mapM getSymbol args <*> sexprToAST body)
-- Variable Definition
handleDefine [SSymbol name, body] = TDefineVariable <$> Right name <*> sexprToAST body
-- Function Definition
handleDefine [SList (SSymbol name : args), body] = TDefineFunction <$> Right name <*> (TLambda <$> mapM getSymbol args <*> sexprToAST body)
handleDefine _ = Left errDef

-- Define

-- Will be used in execute or else
catchDefine :: Either AstError Ast -> Either AstError Ast
catchDefine (Right (TDefineVariable {})) = Left errDefCtx
catchDefine (Right (TDefineFunction {})) = Left errDefCtx
catchDefine (Right (TVariableCall "define")) = Left errDefCtx
catchDefine x = x
