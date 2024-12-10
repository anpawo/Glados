{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Parser
-}

module Lisp.Vm (test_vm) where

-- Base types
data Value = IntVal Int | BoolVal Bool
  deriving (Show, Eq)

-- Oper
data Op = Add | Sub | Mul | Div
  deriving (Show, Eq)

-- Insts
data Inst = Push Value
          | Call Op
          | Ret
  deriving (Show, Eq)

-- Aliases
type Stack = [Value]
type Insts = [Inst]

exec :: Insts -> Stack -> Either String Value
exec [] stack = Left "Error: No ret instruction ?"
exec (Push v : rest) stack = exec rest (v : stack)
exec (Call op : rest) (v1 : v2 : stack) = 
  let result = case (op, v1, v2) of
        (Add, IntVal a, IntVal b) -> Right (IntVal (a + b))
        (Sub, IntVal a, IntVal b) -> Right (IntVal (a - b))
        (Mul, IntVal a, IntVal b) -> Right (IntVal (a * b))
        (Div, IntVal _ , IntVal 0) -> Left "Error: You cant divide by 0"
        (Div, IntVal a, IntVal b) -> Right (IntVal (a `div` b))
        _ -> Left "Error: You cant do that"
  in case result of
       Left err -> Left err
       Right res -> exec rest (res : stack)
exec (Ret : _) (v : _) = Right v
exec _ _ = Left "Error: Invalid program or stack state"

-- TEST VM
test_vm :: IO ()
test_vm = do
  -- Expl 1:
  let program1 = [Push (IntVal 42), Ret]
  print $ exec program1 []

  -- Expl 4:
  let program2 = [Push (IntVal 10), Push (IntVal 52), Call Sub, Ret]
  print $ exec program2 []

  -- Expl 3:
  let program3 = [Push (IntVal 0), Push (IntVal 42), Call Div, Ret]
  print $ exec program3 []

  -- Expl 2:
  let program4 = [Push (IntVal 10), Push (BoolVal True), Call Add, Ret]
  print $ exec program4 []
