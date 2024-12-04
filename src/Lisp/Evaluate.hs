{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Evaluate
-}

module Lisp.Evaluate (evalAst) where

import Data.List (find)
import Lisp.Ast (Ast (..), Ctx)

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
evalAst ctx (TVariableCall name) = retrieveVariable ctx name >>= evalAst ctx
evalAst ctx (TDefineVariable name body) = (,) <$> Right TVoid <*> defineVariable ctx name body
-- evalAst _ (TDefineFunction {}) = print "" >> return (Left "todo")
-- evalAst _ (TIf {}) = print "" >> return (Left "todo")
-- evalAst _ (TFunctionCall {}) = print "" >> return (Left "todo")
evalAst _ _ = Left "todo: evalAst"

eval' :: Ctx -> Ast -> Either EvalErr (Ast, Ctx)
eval' ctx x@(TLambda {}) = {-- evalAst--} (,) <$> functionEval ctx x <*> Right ctx
eval' ctx x = evalAst ctx x

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
