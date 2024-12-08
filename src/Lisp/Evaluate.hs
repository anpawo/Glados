{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Evaluate
-}

module Lisp.Evaluate (evalAst) where

import Data.Fixed (mod')
import Data.List (find)
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
evalAst ctx TLambda {} = Right (TLambda [] TVoid, ctx) -- this is a lambda assigned to nothing, may be passed as param to a function like: (define (f1orf2 cond v f1 f2) (if cond (f1 v) (f2 v))); (f1orf2 #t 1 (lambda (x) (+ x 1)) (lambda (x) (+ x 2)))
evalAst ctx (TLambdaFly args lambda) = (,) <$> lambdaEval "lambda" ctx args lambda <*> Right ctx -- this is a lambda called when created: ((lambda (a b) (+ a b)) 5 4) => 9
evalAst ctx (TDefineVariable name body) = (,) <$> Right TVoid <*> defineVariable ctx name body
evalAst ctx (TDefineFunction name body) = (,) <$> Right TVoid <*> defineFunction ctx name body
evalAst ctx (TIf cond_ then_ else_) = (,) <$> ifEval ctx cond_ then_ else_ <*> Right ctx
evalAst ctx (TCond conditions) = (,) <$> condEval ctx conditions <*> Right ctx
evalAst ctx (TVariableCall name) = (,) <$> (retrieveVariable ctx name >>= Right) <*> Right ctx
evalAst ctx (TFunctionCall name args) = (,) <$> functionEval ctx name args <*> Right ctx

condEval :: Ctx -> [(Ast, Ast)] -> Either EvalErr Ast
condEval _ [] = Left $ errImpossible "empty cond"
condEval ctx [(cond, body)] =
  case evalAst ctx cond >>= (Right . fst) of
    Right (TBool False) -> Right TVoid
    Right _ -> evalAst ctx body >>= Right . fst
    Left err -> Left err
condEval ctx ((cond, body) : rst) =
  case evalAst ctx cond >>= (Right . fst) of
    Right (TBool False) -> condEval ctx rst
    Right _ -> evalAst ctx body >>= Right . fst
    Left err -> Left err

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

lambdaEval :: String -> Ctx -> Args -> Ast -> Either EvalErr Ast
lambdaEval fnNameIfOne _ eargs (TLambda args _) | length eargs /= length args = Left $ errNumberArgs fnNameIfOne
lambdaEval _ ctx eargs (TLambda args body) = combineContext ctx eargs args >>= (`evalAst` body) >>= (Right . fst)
lambdaEval fnNameIfOne _ eargs ast = Left $ errImpossible $ "lambdaeval without a lambda: " ++ show fnNameIfOne ++ ", " ++ show eargs ++ ", " ++ show ast

combineContext :: Ctx -> Args -> [String] -> Either EvalErr Ctx
combineContext ctx eargs args = combinedContext
  where
    newValues :: Either EvalErr Ctx
    newValues = assignValues ctx eargs args

    combinedContext :: Either EvalErr Ctx
    combinedContext = newValues >>= Right . removeDouble ctx

removeDouble :: Ctx -> Ctx -> Ctx
removeDouble oldCtx [] = oldCtx
removeDouble oldCtx (x@(TVariable name _) : rst) = (:) x $ removeDouble (filter (not . sameName name) oldCtx) rst
removeDouble oldCtx (x@(TFunction name _) : rst) = (:) x $ removeDouble (filter (not . sameName name) oldCtx) rst
removeDouble oldCtx (x : rst) = (:) x $ removeDouble oldCtx rst

assignValues :: Ctx -> Args -> [String] -> Either EvalErr Ctx
assignValues _ _ [] = Right []
assignValues _ [] _ = Right [] -- should never happend because we checked before if the number of args were equivalent
assignValues ctx (b@(TLambda {}) : valueRst) (vName : nameRst) = (:) <$> (TFunction <$> Right vName <*> Right b) <*> assignValues ctx valueRst nameRst
assignValues ctx (vValue : valueRst) (vName : nameRst) = (:) <$> (TVariable <$> Right vName <*> (evalAst ctx vValue >>= Right . fst)) <*> assignValues ctx valueRst nameRst

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
    Just (TVariable vName vBody) -> Left $ errNonProcedure "name:" ++ vName ++ ", body:" ++ show vBody
    Just (TFunction fnName lambda) -> lambdaEval fnName ctx args lambda
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
    evalBuiltin (TInt 0) = Right $ TBool True
    evalBuiltin (TInt _) = Right $ TBool False
    evalBuiltin (TFloat 0.0) = Right $ TBool True
    evalBuiltin (TFloat _) = Right $ TBool False
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
-- Tests
-- Fix all TODO
-- Better Error Handling (no define inside anything)
-- add the fractions
-- history command and maybe left/right key
