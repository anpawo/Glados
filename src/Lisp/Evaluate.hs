{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Evaluate
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lisp.Evaluate (evalAst) where

import Data.Fixed (mod')
import Data.List (elemIndex, find, nub)
-- import Debug.Trace (trace)
import Lisp.Ast (Ast (..), Ctx, trueIfTruthy)
import Lisp.ErrorMessage
  ( errImpossible,
    errNonProcedure,
    errNumberArgs,
    errTypeArgs,
    errUnboundVar,
  )

type EvalErr = String

type Args = [Ast]

evalAst :: Ctx -> Ast -> Either EvalErr (Ast, Ctx)
evalAst _ (TFunction {}) = Left $ errImpossible "eval of TFunction" -- only part of the context
evalAst _ (TVariable {}) = Left $ errImpossible "eval of TVariable" -- only part of the context
evalAst ctx x@(TInt {}) = Right (x, ctx)
evalAst ctx x@(TFraction {}) = Right (x, ctx)
evalAst ctx x@(TFloat {}) = Right (x, ctx)
evalAst ctx x@(TBool {}) = Right (x, ctx)
evalAst ctx x@TVoid = Right (x, ctx)
evalAst ctx x@(TString {}) = Right (x, ctx)
evalAst ctx TLambda {} = Right (TLambda [] TVoid, ctx) -- this is a lambda assigned to nothing
evalAst ctx (TLambdaFly args lambda) = (,) <$> lambdaEval ctx args lambda <*> Right ctx -- this is a lambda called when created
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
    _ -> Left $ errImpossible "we found the variable but its not there anymore"

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
lambdaEval _ eargs ast = Left $ errImpossible $ "lambdaeval without a lambda: " ++ show eargs ++ ", " ++ show ast

combineContext :: Ctx -> Args -> [String] -> Ast -> Either EvalErr Ctx
combineContext ctx eargs args ast = updateCtxWSymbol ctx eargs args symbols
  where
    symbols = nub $ retrieveAllSymbols ast

updateCtxWSymbol :: Ctx -> Args -> [String] -> [String] -> Either EvalErr Ctx
updateCtxWSymbol ctx _ _ [] = Right ctx
updateCtxWSymbol ctx eargs args (s : symbols) =
  case elemIndex s args of
    Nothing -> updateCtxWSymbol ctx eargs args symbols
    Just index -> case eargs !! index of
      x@(TLambda {}) -> updateCtxWSymbol (TFunction s x : newCtx) eargs args symbols
      (TVariableCall name) | isFunction ctx name -> updateCtxWSymbol (TFunction s (findFunction ctx name) : newCtx) eargs args symbols
      x -> case evalAst ctx x of
        Left err -> Left err
        Right (v, _) -> updateCtxWSymbol (TVariable s v : newCtx) eargs args symbols
      where
        newCtx = filter (not . sameName s) ctx

findFunction :: Ctx -> String -> Ast
findFunction [] _ = TVoid
findFunction ((TFunction name' body) : rst) name
  | name == name' = body
  | otherwise = findFunction rst name
findFunction (_ : rst) name = findFunction rst name

isFunction :: Ctx -> String -> Bool
isFunction [] _ = False
isFunction ((TFunction name' _) : rst) name
  | name == name' = True
  | otherwise = isFunction rst name
isFunction (_ : rst) name = isFunction rst name

retrieveAllSymbols :: Ast -> [String]
retrieveAllSymbols (TVariableCall name) = [name]
retrieveAllSymbols (TFunctionCall name body) = name : concatMap retrieveAllSymbols body
retrieveAllSymbols (TIf c t e) = retrieveAllSymbols c ++ retrieveAllSymbols t ++ retrieveAllSymbols e
retrieveAllSymbols _ = []

functionEval :: Ctx -> String -> Args -> Either EvalErr Ast
functionEval ctx "eq?" args = builtinEq ctx args
functionEval ctx "=" args = builtinEq ctx args
functionEval ctx "<" args = builtinLT ctx args
functionEval ctx "+" args = builtinAdd ctx args
functionEval ctx "-" args = builtinSub ctx args
functionEval ctx "*" args = builtinMul ctx args
functionEval ctx "div" args = builtinDiv ctx args
functionEval ctx "/" args = builtinDiv ctx args
functionEval ctx "mod" args = builtinMod ctx args
functionEval ctx "%" args = builtinMod ctx args
functionEval ctx name args =
  case find (sameName name) ctx of
    Nothing -> Left $ errUnboundVar name
    Just (TVariable {}) -> Left $ errNonProcedure name
    Just (TFunction _ lambda) -> lambdaEval ctx args lambda
    _ -> Left $ errImpossible "we found the variable but it's not there anymore"

simpl :: Ast -> Ast
simpl (TFraction n1 d1)
  | d2 == 1 = TInt n2
  | n2 == 0 = TInt 0
  | otherwise = TFraction n2 d2
  where
    n2 = n1 `div` pgcd
    d2 = d1 `div` pgcd
    pgcd = gcd n1 d1

flt :: Ast -> Float
flt (TFraction n d) = fromIntegral n / fromIntegral d
flt (TInt x) = fromIntegral x

builtinEq :: Ctx -> Args -> Either EvalErr Ast
builtinEq ctx [l, r] = ((,) <$> (evalAst ctx l >>= Right . fst) <*> (evalAst ctx r >>= Right . fst)) >>= evalBuiltin
  where
    evalBuiltin (a@(TInt {}), b@(TFraction {})) = Right $ TBool (flt a == flt b)
    evalBuiltin (a@(TFraction {}), b@(TInt {})) = Right $ TBool (flt a == flt b)
    evalBuiltin (a@(TFraction {}), TFloat b) = Right $ TBool (flt a == b)
    evalBuiltin (TFloat a, b@(TFraction {})) = Right $ TBool (a == flt b)
    evalBuiltin (a@(TInt {}), TFloat b) = Right $ TBool (flt a == b)
    evalBuiltin (TFloat a, b@(TInt {})) = Right $ TBool (a == flt b)
    evalBuiltin (a, b) = Right $ TBool (a == b)
builtinEq _ _ = Left $ errNumberArgs "eq?"

builtinLT :: Ctx -> Args -> Either EvalErr Ast
builtinLT ctx [l, r] = ((,) <$> (evalAst ctx l >>= Right . fst) <*> (evalAst ctx r >>= Right . fst)) >>= evalBuiltin
  where
    evalBuiltin (TFraction n1 d1, TFraction n2 d2) = Right $ TBool (n1 * d2 < d1 * n2)
    evalBuiltin (TFraction n1 d1, TInt n2) = Right $ TBool (n1 < d1 * n2)
    evalBuiltin (a@(TFraction {}), TFloat b) = Right $ TBool (flt a < b)
    evalBuiltin (TInt a, TInt b) = Right $ TBool (a < b)
    evalBuiltin (TInt n1, TFraction n2 d2) = Right $ TBool (n1 * d2 < n2)
    evalBuiltin (a@(TInt {}), TFloat b) = Right $ TBool (flt a < b)
    evalBuiltin (TFloat a, TFloat b) = Right $ TBool (a < b)
    evalBuiltin (TFloat a, b@(TFraction {})) = Right $ TBool (a < flt b)
    evalBuiltin (TFloat a, b@(TInt {})) = Right $ TBool (a < flt b)
    evalBuiltin _ = Left $ errTypeArgs "<" "bool"
builtinLT _ _ = Left $ errNumberArgs "<"

builtinAdd :: Ctx -> Args -> Either EvalErr Ast
builtinAdd ctx [l, r] = ((,) <$> (evalAst ctx l >>= Right . fst) <*> (evalAst ctx r >>= Right . fst)) >>= evalBuiltin
  where
    evalBuiltin (TFraction n1 d1, TFraction n2 d2) = Right $ simpl $ TFraction (n1 * d2 + d1 * n2) (d1 * d2)
    evalBuiltin (TFraction n1 d1, TInt n2) = Right $ simpl $ TFraction (n1 + d1 * n2) d1
    evalBuiltin (a@(TFraction {}), TFloat b) = Right $ TFloat $ flt a + b
    evalBuiltin (TInt a, TInt b) = Right $ TInt (a + b)
    evalBuiltin (TInt n1, TFraction n2 d2) = Right $ simpl $ TFraction (n1 * d2 + n2) d2
    evalBuiltin (a@(TInt {}), TFloat b) = Right $ TFloat (flt a + b)
    evalBuiltin (TFloat a, TFloat b) = Right $ TFloat (a + b)
    evalBuiltin (TFloat a, b@(TFraction {})) = Right $ TFloat (a + flt b)
    evalBuiltin (TFloat a, b@(TInt {})) = Right $ TFloat (a + flt b)
    evalBuiltin _ = Left $ errTypeArgs "+" "int/float/fraction"
builtinAdd _ _ = Left $ errNumberArgs "+"

builtinSub :: Ctx -> Args -> Either EvalErr Ast
builtinSub ctx [l, r] = ((,) <$> (evalAst ctx l >>= Right . fst) <*> (evalAst ctx r >>= Right . fst)) >>= evalBuiltin
  where
    evalBuiltin (TFraction n1 d1, TFraction n2 d2) = Right $ simpl $ TFraction (n1 * d2 - d1 * n2) (d1 * d2)
    evalBuiltin (TFraction n1 d1, TInt n2) = Right $ simpl $ TFraction (n1 - d1 * n2) d1
    evalBuiltin (a@(TFraction {}), TFloat b) = Right $ TFloat $ flt a - b
    evalBuiltin (TInt a, TInt b) = Right $ TInt (a - b)
    evalBuiltin (TInt n1, TFraction n2 d2) = Right $ simpl $ TFraction (n1 * d2 - n2) d2
    evalBuiltin (a@(TInt {}), TFloat b) = Right $ TFloat (flt a - b)
    evalBuiltin (TFloat a, TFloat b) = Right $ TFloat (a - b)
    evalBuiltin (TFloat a, b@(TFraction {})) = Right $ TFloat (a - flt b)
    evalBuiltin (TFloat a, b@(TInt {})) = Right $ TFloat (a - flt b)
    evalBuiltin _ = Left $ errTypeArgs "-" "int/float/fraction"
builtinSub _ _ = Left $ errNumberArgs "-"

builtinMul :: Ctx -> Args -> Either EvalErr Ast
builtinMul ctx [l, r] = ((,) <$> (evalAst ctx l >>= Right . fst) <*> (evalAst ctx r >>= Right . fst)) >>= evalBuiltin
  where
    evalBuiltin (TFraction n1 d1, TFraction n2 d2) = Right $ simpl $ TFraction (n1 * d2) (n2 * d1)
    evalBuiltin (TFraction n1 d1, TInt n2) = Right $ simpl $ TFraction (n1 - d1 * n2) d1
    evalBuiltin (a@(TFraction {}), TFloat b) = Right $ TFloat $ flt a - b
    evalBuiltin (TInt a, TInt b) = Right $ TInt (a - b)
    evalBuiltin (TInt n1, TFraction n2 d2) = Right $ simpl $ TFraction (n1 * d2 - n2) d2
    evalBuiltin (a@(TInt {}), TFloat b) = Right $ TFloat (flt a - b)
    evalBuiltin (TFloat a, TFloat b) = Right $ TFloat (a - b)
    evalBuiltin (TFloat a, b@(TFraction {})) = Right $ TFloat (a - flt b)
    evalBuiltin (TFloat a, b@(TInt {})) = Right $ TFloat (a - flt b)
    evalBuiltin _ = Left $ errTypeArgs "*" "int/float/fraction"
builtinMul _ _ = Left $ errNumberArgs "*"

builtinDiv :: Ctx -> Args -> Either EvalErr Ast
builtinDiv ctx [l, r] = ((,) <$> (evalAst ctx l >>= Right . fst) <*> (evalAst ctx r >>= Right . fst)) >>= evalBuiltin
  where
    evalBuiltin (_, TInt 0) = Left "Error: Division by 0"
    evalBuiltin (_, TFloat 0.0) = Left "Error: Division by 0"
    -- Fraction Creation
    evalBuiltin (TInt n, TInt d)
      | intdiv * d == n = Right $ TInt intdiv
      | otherwise = Right $ simpl $ TFraction n d
      where
        intdiv = n `div` d
    -- Fraction Creation
    evalBuiltin (a@(TInt {}), TFloat b) = Right $ TFloat (flt a / b)
    evalBuiltin (TInt n1, TFraction n2 d2) = Right $ simpl $ TFraction (n1 * n2) d2
    evalBuiltin (TFloat a, TFloat b) = Right $ TFloat (a / b)
    evalBuiltin (TFloat a, b@(TInt {})) = Right $ TFloat (a / flt b)
    evalBuiltin (TFloat a, b@(TFraction {})) = Right $ TFloat (a / flt b)
    evalBuiltin (TFraction n1 d1, TFraction n2 d2) = Right $ simpl $ TFraction (n1 * n2) (d1 * d2)
    evalBuiltin (a@(TFraction {}), TFloat b) = Right $ TFloat (flt a / b)
    evalBuiltin (TFraction n1 d1, TInt n2) = Right $ simpl $ TFraction (n1 * n2) d1
    evalBuiltin _ = Left $ errTypeArgs "div" "int/float/fraction"
builtinDiv _ _ = Left $ errNumberArgs "div"

builtinMod :: Ctx -> Args -> Either EvalErr Ast
builtinMod ctx [l, r] = ((,) <$> (evalAst ctx l >>= Right . fst) <*> (evalAst ctx r >>= Right . fst)) >>= evalBuiltin
  where
    evalBuiltin (_, TInt 0) = Left "Error: Division by 0"
    evalBuiltin (_, TFloat 0.0) = Left "Error: Division by 0"
    evalBuiltin (TInt a, TInt b) = Right $ TInt (a `mod` b)
    evalBuiltin (a@(TInt {}), TFloat b) = Right $ TFloat (flt a `mod'` b)
    evalBuiltin (a@(TInt {}), b@(TFraction {})) = Right $ TFloat (flt a `mod'` flt b)
    evalBuiltin (TFloat a, TFloat b) = Right $ TFloat (a `mod'` b)
    evalBuiltin (TFloat a, b@(TInt {})) = Right $ TFloat (a `mod'` flt b)
    evalBuiltin (TFloat a, b@(TFraction {})) = Right $ TFloat (a `mod'` flt b)
    evalBuiltin (a@(TFraction {}), b@(TFraction {})) = Right $ TFloat (flt a `mod'` flt b)
    evalBuiltin (a@(TFraction {}), b@(TInt {})) = Right $ TFloat (flt a `mod'` flt b)
    evalBuiltin (a@(TFraction {}), TFloat b) = Right $ TFloat (flt a `mod'` b)
    evalBuiltin _ = Left $ errTypeArgs "mod" "int/float/fraction"
builtinMod _ _ = Left $ errNumberArgs "mod"

-- TODO:
-- Doc
-- Ci (release)
-- Tests
-- Fix all TODO
-- Better Error Handling (no define inside anything)
-- add issues
