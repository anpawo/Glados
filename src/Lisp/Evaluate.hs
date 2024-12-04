{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Evaluate
-}

module Lisp.Evaluate (evalAst) where

import Data.List (find)
import Lisp.Ast (Ast (..), Ctx, trueIfTruthy)

type EvalErr = String

unboundVar :: String -> String
unboundVar name = "Error: variable " ++ name ++ " is not bound"

impossibleCase :: String
impossibleCase = "Impossible: This should never happend."

evalAst :: Ctx -> Ast -> Either EvalErr (Ast, Ctx)
evalAst _ (TFunction {}) = Left impossibleCase -- only part of the context
evalAst _ (TVariable {}) = Left impossibleCase -- only part of the context
evalAst ctx x@(TInt {}) = Right (x, ctx)
evalAst ctx x@(TBool {}) = Right (x, ctx)
evalAst ctx x@TVoid = Right (x, ctx)
evalAst ctx x@(TString {}) = Right (x, ctx)
evalAst ctx TLambda {} = Right (TLambda [] TVoid, ctx) -- TODO: compute the lambda
evalAst ctx (TVariableCall name) = (,) <$> (retrieveVariable ctx name >>= Right) <*> Right ctx
evalAst ctx (TDefineVariable name body) = (,) <$> Right TVoid <*> defineVariable ctx name body
evalAst ctx (TDefineFunction name body) = (,) <$> Right TVoid <*> defineFunction ctx name body
evalAst ctx (TIf c t e) = (,) <$> ifEval ctx c t e <*> Right ctx
-- evalAst _ (TFunctionCall {}) = print "" >> return (Left "todo")
evalAst _ _ = Left "todo: evalAst"

eval' :: Ctx -> Ast -> Either EvalErr (Ast, Ctx)
eval' ctx x@(TLambda {}) = (,) <$> functionEval ctx x <*> Right ctx
eval' ctx x = evalAst ctx x

ifEval :: Ctx -> Ast -> Ast -> Ast -> Either EvalErr Ast
ifEval ctx c t e =
  case evalAst ctx c >>= (Right . trueIfTruthy . fst) of
    Right (TBool True) -> evalAst ctx t >>= Right . fst
    Right _ -> evalAst ctx e >>= Right . fst
    Left err -> Left err

-- \| trueIfTruthy condition = ""
-- \| otherwise = ""

functionEval :: Ctx -> Ast -> Either EvalErr Ast
functionEval _ _ = Left "todo: functionEval"

retrieveVariable :: Ctx -> String -> Either EvalErr Ast
retrieveVariable ctx name =
  case find (sameName name) ctx of
    Nothing -> Left $ unboundVar name
    Just (TVariable _ val) -> Right val
    Just (TFunction name' _) -> Right (TLambda [name'] TVoid)
    _ -> Left impossibleCase -- impossible

sameName :: String -> Ast -> Bool
sameName name (TVariable name' _) = name == name'
sameName name (TFunction name' _) = name == name'
sameName _ _ = False

defineVariable :: Ctx -> String -> Ast -> Either EvalErr Ctx
defineVariable ctx name body = (:) <$> (eval' newCtx body >>= (Right . TVariable name . fst)) <*> Right newCtx
  where
    newCtx = filter (not . sameName name) ctx

defineFunction :: Ctx -> String -> Ast -> Either EvalErr Ctx
defineFunction ctx name body = (:) <$> Right (TFunction name body) <*> Right newCtx
  where
    newCtx = filter (not . sameName name) ctx
