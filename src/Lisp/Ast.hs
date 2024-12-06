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
    trueIfTruthy,
  )
where

import Lisp.ErrorMessage
import Lisp.SExpression (SExpr (..), getSymbol)

data Ast
  = -- basic types && consumed by execute to display
    TInt Int
  | TFraction {fNumerator :: Int, fDenumerator :: Int}
  | TFloat Float
  | TBool Bool
  | TVoid
  | TString String
  | TIf {ifCond :: Ast, ifThen :: Ast, ifElse :: Ast}
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
handleList functionNameAndArgs = handleCall functionNameAndArgs

-- Lambda
handleLambda :: [SExpr] -> Either AstError Ast
handleLambda [] = Left $ errLambda "missing arguments and body"
handleLambda [_] = Left $ errLambda "missing body"
handleLambda [SList args, body] = TLambda <$> mapM getSymbol args <*> sexprToAST body
handleLambda _ = Left $ errLambda "expected (lambda (args) body)"

-- Lambda

-- If
handleIf :: [SExpr] -> Either AstError Ast
handleIf [a, b, c] = TIf <$> (sexprToAST a >>= Right . trueIfTruthy) <*> sexprToAST b <*> sexprToAST c
handleIf [a, b] = TIf <$> (sexprToAST a >>= Right . trueIfTruthy) <*> sexprToAST b <*> Right TVoid
handleIf _ = Left $ errIf "expected (if cond then else)"

trueIfTruthy :: Ast -> Ast
trueIfTruthy TInt {} = TBool True
trueIfTruthy TFloat {} = TBool True
trueIfTruthy TVoid {} = TBool True
trueIfTruthy TString {} = TBool True
trueIfTruthy TLambda {} = TBool True
trueIfTruthy x = x

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
