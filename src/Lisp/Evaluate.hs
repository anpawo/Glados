{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Evaluate
-}

module Lisp.Evaluate (evalAst) where

import Data.Fixed (mod')
import Data.List (elemIndex, find, nub)
import Lisp.Ast (Ast (..), Ctx, trueIfTruthy)
import Lisp.ErrorMessage

type EvalErr = String

type Args = [Ast]

evalAst :: Ctx -> Ast -> Either EvalErr (Ast, Ctx)
evalAst _ (TFunction {}) = Left errImpossible -- only part of the context
evalAst _ (TVariable {}) = Left errImpossible -- only part of the context
evalAst ctx x@(TInt {}) = Right (x, ctx)
evalAst ctx x@(TFloat {}) = Right (x, ctx)
evalAst ctx x@(TBool {}) = Right (x, ctx)
evalAst ctx x@TVoid = Right (x, ctx)
evalAst ctx x@(TString {}) = Right (x, ctx)
evalAst ctx TLambda {} = Right (TLambda [] TVoid, ctx) -- TODO: compute the lambda
evalAst ctx (TDefineVariable name body) = (,) <$> Right TVoid <*> defineVariable ctx name body
evalAst ctx (TDefineFunction name body) = (,) <$> Right TVoid <*> defineFunction ctx name body
evalAst ctx (TIf c t e) = (,) <$> ifEval ctx c t e <*> Right ctx
evalAst ctx (TVariableCall name) = (,) <$> (retrieveVariable ctx name >>= Right) <*> Right ctx
evalAst ctx (TFunctionCall name args) = (,) <$> functionEval ctx name args <*> Right ctx

ifEval :: Ctx -> Ast -> Ast -> Ast -> Either EvalErr Ast
ifEval ctx c t e =
  case evalAst ctx c >>= (Right . trueIfTruthy . fst) of
    Right (TBool True) -> evalAst ctx t >>= Right . fst
    Right _ -> evalAst ctx e >>= Right . fst
    Left err -> Left err

retrieveVariable :: Ctx -> String -> Either EvalErr Ast
retrieveVariable ctx name =
  case find (sameName name) ctx of
    Nothing -> Left $ errUnboundVar name
    Just (TVariable _ val) -> Right val
    Just (TFunction name' _) -> Right (TLambda [name'] TVoid) -- handles the procedure message but is a little trick. may be removed.
    _ -> Left errImpossible

sameName :: String -> Ast -> Bool
sameName name (TVariable name' _) = name == name'
sameName name (TFunction name' _) = name == name'
sameName _ _ = False

defineVariable :: Ctx -> String -> Ast -> Either EvalErr Ctx
defineVariable ctx name body = (:) <$> (evalAst newCtx body >>= (Right . TVariable name . fst)) <*> Right newCtx
  where
    newCtx = filter (not . sameName name) ctx

defineFunction :: Ctx -> String -> Ast -> Either EvalErr Ctx
defineFunction ctx name body = (:) <$> Right (TFunction name body) <*> Right newCtx
  where
    newCtx = filter (not . sameName name) ctx

lambdaEval :: Ctx -> Args -> Ast -> Either EvalErr Ast
lambdaEval _ eargs (TLambda args _) | length eargs /= length args = Left $ errNumberArgs "lambda"
lambdaEval ctx eargs (TLambda args body) = combineContext ctx eargs args body >>= (`evalAst` body) >>= (Right . fst)
lambdaEval _ _ _ = Left errImpossible

combineContext :: Ctx -> Args -> [String] -> Ast -> Either EvalErr Ctx
combineContext ctx eargs args ast = updateCtxWSymbol ctx eargs args symbols
  where
    symbols = nub $ retrieveAllSymbols ast

updateCtxWSymbol :: Ctx -> Args -> [String] -> [String] -> Either EvalErr Ctx
updateCtxWSymbol ctx _ _ [] = Right ctx
updateCtxWSymbol ctx eargs args (s : symbols) = case elemIndex s args of
  Nothing -> updateCtxWSymbol ctx eargs args symbols
  Just index -> case eargs !! index of
    x@(TLambda {}) -> updateCtxWSymbol (TFunction s x : newCtx) eargs args symbols
    x -> case evalAst ctx x of
      Left err -> Left err
      Right (v, _) -> updateCtxWSymbol (TVariable s v : newCtx) eargs args symbols
    where
      newCtx = filter (not . sameName s) ctx

retrieveAllSymbols :: Ast -> [String]
retrieveAllSymbols (TVariableCall name) = [name]
retrieveAllSymbols (TFunctionCall name body) = name : concatMap retrieveAllSymbols body
retrieveAllSymbols (TIf c t e) = retrieveAllSymbols c ++ retrieveAllSymbols t ++ retrieveAllSymbols e
retrieveAllSymbols _ = []

functionEval :: Ctx -> String -> Args -> Either EvalErr Ast
functionEval ctx "eq?" args = builtinEq ctx args
functionEval ctx "==" args = builtinEq ctx args
functionEval ctx "<" args = builtinLT ctx args
functionEval ctx "+" args = builtinAdd ctx args
functionEval ctx "-" args = builtinSub ctx args
functionEval ctx "*" args = builtinMul ctx args
functionEval ctx "div" args = builtinDiv ctx args
functionEval ctx "/" args = builtinDiv ctx args
functionEval ctx "mod" args = builtinMod ctx args
functionEval ctx "//" args = builtinMod ctx args
functionEval ctx name args =
  case find (sameName name) ctx of
    Nothing -> Left $ errUnboundVar name
    Just (TVariable {}) -> Left $ errNonProcedure name
    Just (TFunction _ lambda) -> lambdaEval ctx args lambda
    _ -> Left errImpossible

builtinEq :: Ctx -> Args -> Either EvalErr Ast
builtinEq ctx [l, r] = ((,) <$> (evalAst ctx l >>= Right . fst) <*> (evalAst ctx r >>= Right . fst)) >>= evalBuiltin
  where
    evalBuiltin (TInt a, TFloat b) = Right $ TBool (fromIntegral a == b)
    evalBuiltin (TFloat a, TInt b) = Right $ TBool (a == fromIntegral b)
    evalBuiltin (a, b) = Right $ TBool (a == b)
builtinEq _ _ = Left $ errNumberArgs "eq?"

builtinLT :: Ctx -> Args -> Either EvalErr Ast
builtinLT ctx [l, r] = ((,) <$> (evalAst ctx l >>= Right . fst) <*> (evalAst ctx r >>= Right . fst)) >>= evalBuiltin
  where
    evalBuiltin (TInt a, TInt b) = Right $ TBool (a < b)
    evalBuiltin (TFloat a, TFloat b) = Right $ TBool (a < b)
    evalBuiltin (TFloat a, TInt b) = Right $ TBool (a < fromIntegral b)
    evalBuiltin (TInt a, TFloat b) = Right $ TBool (fromIntegral a < b)
    evalBuiltin _ = Left $ errTypeArgs "<" "bool"
builtinLT _ _ = Left $ errNumberArgs "<"

builtinAdd :: Ctx -> Args -> Either EvalErr Ast
builtinAdd ctx [l, r] = ((,) <$> (evalAst ctx l >>= Right . fst) <*> (evalAst ctx r >>= Right . fst)) >>= evalBuiltin
  where
    evalBuiltin (TInt a, TInt b) = Right $ TInt (a + b)
    evalBuiltin (TFloat a, TFloat b) = Right $ TFloat (a + b)
    evalBuiltin (TFloat a, TInt b) = Right $ TFloat (a + fromIntegral b)
    evalBuiltin (TInt a, TFloat b) = Right $ TFloat (fromIntegral a + b)
    evalBuiltin _ = Left $ errTypeArgs "+" "int or float"
builtinAdd _ _ = Left $ errNumberArgs "+"

builtinSub :: Ctx -> Args -> Either EvalErr Ast
builtinSub ctx [l, r] = ((,) <$> (evalAst ctx l >>= Right . fst) <*> (evalAst ctx r >>= Right . fst)) >>= evalBuiltin
  where
    evalBuiltin (TInt a, TInt b) = Right $ TInt (a - b)
    evalBuiltin (TFloat a, TFloat b) = Right $ TFloat (a - b)
    evalBuiltin (TFloat a, TInt b) = Right $ TFloat (a - fromIntegral b)
    evalBuiltin (TInt a, TFloat b) = Right $ TFloat (fromIntegral a - b)
    evalBuiltin _ = Left $ errTypeArgs "-" "int or float"
builtinSub _ _ = Left $ errNumberArgs "-"

builtinMul :: Ctx -> Args -> Either EvalErr Ast
builtinMul ctx [l, r] = ((,) <$> (evalAst ctx l >>= Right . fst) <*> (evalAst ctx r >>= Right . fst)) >>= evalBuiltin
  where
    evalBuiltin (TInt a, TInt b) = Right $ TInt (a * b)
    evalBuiltin (TFloat a, TFloat b) = Right $ TFloat (a * b)
    evalBuiltin (TFloat a, TInt b) = Right $ TFloat (a * fromIntegral b)
    evalBuiltin (TInt a, TFloat b) = Right $ TFloat (fromIntegral a * b)
    evalBuiltin _ = Left $ errTypeArgs "*" "int or float"
builtinMul _ _ = Left $ errNumberArgs "*"

builtinDiv :: Ctx -> Args -> Either EvalErr Ast
builtinDiv ctx [l, r] = ((,) <$> (evalAst ctx l >>= Right . fst) <*> (evalAst ctx r >>= Right . fst)) >>= evalBuiltin
  where
    evalBuiltin (_, TInt 0) = Left "Error: Division by 0"
    evalBuiltin (_, TFloat 0.0) = Left "Error: Division by 0"
    evalBuiltin (TInt a, TInt b) = Right $ TInt (a `div` b)
    evalBuiltin (TFloat a, TFloat b) = Right $ TFloat (a / b)
    evalBuiltin (TFloat a, TInt b) = Right $ TFloat (a / fromIntegral b)
    evalBuiltin (TInt a, TFloat b) = Right $ TFloat (fromIntegral a / b)
    evalBuiltin _ = Left $ errTypeArgs "div" "int or float"
builtinDiv _ _ = Left $ errNumberArgs "div"

builtinMod :: Ctx -> Args -> Either EvalErr Ast
builtinMod ctx [l, r] = ((,) <$> (evalAst ctx l >>= Right . fst) <*> (evalAst ctx r >>= Right . fst)) >>= evalBuiltin
  where
    evalBuiltin (_, TInt 0) = Left "Error: Division by 0"
    evalBuiltin (_, TFloat 0.0) = Left "Error: Division by 0"
    evalBuiltin (TInt a, TInt b) = Right $ TInt (a `mod` b)
    evalBuiltin (TFloat a, TFloat b) = Right $ TFloat (a `mod'` b)
    evalBuiltin (TFloat a, TInt b) = Right $ TFloat (a `mod'` fromIntegral b)
    evalBuiltin (TInt a, TFloat b) = Right $ TFloat (fromIntegral a `mod'` b)
    evalBuiltin _ = Left $ errTypeArgs "mod" "int or float"
builtinMod _ _ = Left $ errNumberArgs "mod"
