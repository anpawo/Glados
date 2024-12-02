{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Compute
-}

module Lisp.Compute (testCompute) where

import Lisp.Ast (Ast (..))


-- Abstract Syntax Tree
-- computeAst :: [Ast] -> Ast -> IO Ast
-- computeAst _ x = do
--     putStrLn "Processing AST..."
--     return x

-- compute func
compute :: Ast -> Int
compute (TInt n) = n
compute (TSymbol _) = error "Symbol not resolved yet"
compute (TString _) = error "Cannot compute strings directly"
compute (TBool _) = error "Cannot compute booleans directly"
compute TVoid = error "Cannot compute void"
compute (TVariable _ varBody) = compute varBody -- if other expr
compute (TFunction _ _ _) = error "Cannot compute functions directly"
compute (TCall "+" [TInt x, TInt y]) = x + y
compute (TCall "-" [TInt x, TInt y]) = x - y
compute (TCall "*" [TInt x, TInt y]) = x * y
compute (TCall "/" [TInt x, TInt y]) = x `div` y
compute (TCall op _) = error ("Operation " ++ op ++ " not supported or wrong number of arguments")

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

-- fo,nction test
testCompute :: IO ()
testCompute = do
  let additionResult = compute testAddition
  let substractionResult = compute testSubstraction
  let multiplicationResult = compute testMultiplication
  let divisionResult = compute testDivision
  
  putStrLn $ "Result of addition: " ++ show additionResult
  putStrLn $ "Result of subtraction: " ++ show substractionResult
  putStrLn $ "Result of multiplication: " ++ show multiplicationResult
  putStrLn $ "Result of division: " ++ show divisionResult