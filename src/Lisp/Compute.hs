{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Compute
-}

module Lisp.Compute (testCompute, compute) where

import Lisp.Ast (Ast (..))


-- Abstract Syntax Tree
-- computeAst :: [Ast] -> Ast -> IO Ast
-- computeAst _ x = do
--     putStrLn "Processing AST..."
--     return x

-- compute func
compute :: Ast -> Either String Ast
compute (TInt n) = Right (TInt n)
compute (TSymbol _) = Left "Symbol not resolved yet"
compute (TString _) = Left "Cannot compute strings directly"
compute (TBool _) = Left "Cannot compute booleans directly"
compute TVoid = Left "Cannot compute void"
compute (TVariable _ varBody) = compute varBody
compute (TFunction _ _ _) = Left "Cannot compute functions directly"
compute (TCall "+" [TInt x, TInt y]) = Right (TInt (x + y))
compute (TCall "-" [TInt x, TInt y]) = Right (TInt (x - y))
compute (TCall "*" [TInt x, TInt y]) = Right (TInt (x * y))
compute (TCall "/" [TInt x, TInt 0]) = Left (show x ++ " can't be divided by 0")
compute (TCall "/" [TInt x, TInt y]) = Right (TInt (x `div` y))
compute (TCall op _) = Left ("Operation " ++ op ++ " not supported or wrong number of arguments")

-- expl add
testAddition :: Ast
testAddition = TCall "+" [TInt 5, TInt 9]

-- expl -
testSubstraction :: Ast
testSubstraction = TCall "-" [TInt 10, TInt 4]

-- expl *
testMultiplication :: Ast
testMultiplication = TCall "*" [TInt 3, TInt 7]

-- expl div
testDivision :: Ast
testDivision = TCall "/" [TInt 20, TInt 4]

-- expl int
testInteger :: Ast
testInteger = TInt 10

-- expl int
testVariable :: Ast
testVariable = TVariable "Test" (TInt 185)

-- fo,nction test
testCompute :: IO ()
testCompute = do
  let additionResult = compute testAddition
  let substractionResult = compute testSubstraction
  let multiplicationResult = compute testMultiplication
  let divisionResult = compute testDivision
  let intResult = compute testInteger
  let variableResult = compute testVariable
  
  putStrLn $ "Result of addition: " ++ show additionResult
  putStrLn $ "Result of subtraction: " ++ show substractionResult
  putStrLn $ "Result of multiplication: " ++ show multiplicationResult
  putStrLn $ "Result of division: " ++ show divisionResult
  putStrLn $ "Result of int: " ++ show intResult
  putStrLn $ "Result of int: " ++ show variableResult