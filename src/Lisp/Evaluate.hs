{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Evaluate
-}

module Lisp.Evaluate (evalAst) where

import Data.Fixed (mod')
import Data.List (elemIndex, find, nub)
-- import Debug.Trace (trace)
import Lisp.Ast (Ast (..), Ctx)
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
evalAst ctx x@(TFloat {}) = Right (x, ctx)
evalAst ctx x@(TBool {}) = Right (x, ctx)
evalAst ctx x@TVoid = Right (x, ctx)
evalAst ctx x@(TString {}) = Right (x, ctx)
evalAst ctx TLambda {} = Right (TLambda [] TVoid, ctx) -- this is a lambda assigned to nothing
evalAst ctx (TLambdaFly args lambda) = (,) <$> lambdaEval ctx args lambda <*> Right ctx -- this is a lambda called when created
evalAst ctx (TDefineVariable name body) = (,) <$> Right TVoid <*> defineVariable ctx name body
evalAst ctx (TDefineFunction name body) = (,) <$> Right TVoid <*> defineFunction ctx name body
evalAst ctx (TIf cond_ then_ else_) = (,) <$> ifEval ctx cond_ then_ else_ <*> Right ctx
evalAst ctx (TVariableCall name) = (,) <$> (retrieveVariable ctx name >>= Right) <*> Right ctx
evalAst ctx (TFunctionCall name args) = (,) <$> functionEval ctx name args <*> Right ctx

ifEval :: Ctx -> Ast -> Ast -> Ast -> Either EvalErr Ast
ifEval ctx cond_ then_ else_ =
  case evalAst ctx cond_ >>= (Right . fst) of
    Right (TBool False) -> evalAst ctx else_ >>= Right . fst
    Right _ -> evalAst ctx then_ >>= Right . fst
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
functionEval ctx "zero?" args = builtinZero ctx args
functionEval ctx "=" args = builtinEq ctx args
functionEval ctx "<" args = builtinLT ctx args
functionEval ctx "+" args = builtinAdd ctx args
functionEval ctx "-" args = builtinSub ctx args
functionEval ctx "*" args = builtinMul ctx args
functionEval ctx "div" args = builtinDiv ctx args
functionEval ctx "/" args = builtinDiv ctx args
functionEval ctx "mod" args = builtinMod ctx args
functionEval ctx "string-append" args = builtinStringAppend ctx args
functionEval ctx name args =
  case find (sameName name) ctx of
    Nothing -> Left $ errUnboundVar name
    Just (TVariable {}) -> Left $ errNonProcedure name
    Just (TFunction _ lambda) -> lambdaEval ctx args lambda
    _ -> Left $ errImpossible "we found the variable but it's not there anymore"

builtinStringAppend :: Ctx -> Args -> Either EvalErr Ast
builtinStringAppend ctx args = consumeString args >>= (Right . TString)
  where
    consumeString [] = Right ""
    consumeString (x : rst) = case evalAst ctx x of
      Right (TString s, _) -> (++) <$> Right s <*> consumeString rst
      Right _ -> Left $ errTypeArgs "string-append" "string"
      Left err -> Left err

builtinZero :: Ctx -> Args -> Either EvalErr Ast
builtinZero ctx [x] = (evalAst ctx x >>= (Right . fst)) >>= evalBuiltin
  where
    evalBuiltin (TInt a)
      | a == 0 = Right $ TBool True
      | otherwise = Right $ TBool False
    evalBuiltin (TFloat a)
      | a == 0.0 = Right $ TBool True
      | otherwise = Right $ TBool False
    evalBuiltin _ = Left $ errTypeArgs "zero?" "number"
builtinZero _ _ = Left $ errNumberArgs "zero?"

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
    evalBuiltin _ = Left $ errTypeArgs "+" "number"
builtinAdd _ _ = Left $ errNumberArgs "+"

builtinSub :: Ctx -> Args -> Either EvalErr Ast
builtinSub ctx [l, r] = ((,) <$> (evalAst ctx l >>= Right . fst) <*> (evalAst ctx r >>= Right . fst)) >>= evalBuiltin
  where
    evalBuiltin (TInt a, TInt b) = Right $ TInt (a - b)
    evalBuiltin (TFloat a, TFloat b) = Right $ TFloat (a - b)
    evalBuiltin (TFloat a, TInt b) = Right $ TFloat (a - fromIntegral b)
    evalBuiltin (TInt a, TFloat b) = Right $ TFloat (fromIntegral a - b)
    evalBuiltin _ = Left $ errTypeArgs "-" "number"
builtinSub _ _ = Left $ errNumberArgs "-"

builtinMul :: Ctx -> Args -> Either EvalErr Ast
builtinMul ctx [l, r] = ((,) <$> (evalAst ctx l >>= Right . fst) <*> (evalAst ctx r >>= Right . fst)) >>= evalBuiltin
  where
    evalBuiltin (TInt a, TInt b) = Right $ TInt (a * b)
    evalBuiltin (TFloat a, TFloat b) = Right $ TFloat (a * b)
    evalBuiltin (TFloat a, TInt b) = Right $ TFloat (a * fromIntegral b)
    evalBuiltin (TInt a, TFloat b) = Right $ TFloat (fromIntegral a * b)
    evalBuiltin _ = Left $ errTypeArgs "*" "number"
builtinMul _ _ = Left $ errNumberArgs "*"

builtinDiv :: Ctx -> Args -> Either EvalErr Ast
builtinDiv ctx [l, r] = ((,) <$> (evalAst ctx l >>= Right . fst) <*> (evalAst ctx r >>= Right . fst)) >>= evalBuiltin
  where
    evalBuiltin (_, TInt 0) = Left "Error: Division by 0"
    evalBuiltin (_, TFloat 0.0) = Left "Error: Division by 0"
    evalBuiltin (TInt a, TInt b)
      | fromIntegral intdiv == fltdiv = Right $ TInt intdiv
      | otherwise = Right $ TFloat fltdiv
      where
        intdiv :: Int
        intdiv = a `div` b

        fltdiv :: Float
        fltdiv = fromIntegral a / fromIntegral b
    evalBuiltin (TFloat a, TFloat b) = Right $ TFloat (a / b)
    evalBuiltin (TFloat a, TInt b) = Right $ TFloat (a / fromIntegral b)
    evalBuiltin (TInt a, TFloat b) = Right $ TFloat (fromIntegral a / b)
    evalBuiltin _ = Left $ errTypeArgs "div" "number"
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
    evalBuiltin _ = Left $ errTypeArgs "mod" "number"
builtinMod _ _ = Left $ errNumberArgs "mod"

-- TODO:
-- Doc
-- Ci (release)
-- Tests
-- Fix all TODO
-- Better Error Handling (no define inside anything)
-- add the fractions
-- add zero?
-- cond
-- history command and maybe left/right key
