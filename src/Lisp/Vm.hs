{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Parser
-}

module Lisp.Vm (test_vm) where

-- Def base types
data Value = IntVal Int | BoolVal Bool
  deriving (Show, Eq)

-- Def oper
data Op = Add | Sub | Mul | Div
  deriving (Show, Eq)

-- Def instructions
data Inst = Push Value       -- Here you can add a value on the stack
          | Call Op          -- Here you can add an operator on the stack
          | Ret              -- Return the value at the top of the stack
  deriving (Show, Eq)

-- Def aliases
type Stack = [Value]
type Insts = [Inst]

-- Func to execute the stack
exec :: Insts -> Stack -> Value
exec [] stack = error "Program terminated without Ret instruction"
exec (Push v : rest) stack = exec rest (v : stack)
exec (Call op : rest) (v1 : v2 : stack) = 
  let result = case (op, v1, v2) of
        (Add, IntVal a, IntVal b) -> IntVal (a + b)
        (Sub, IntVal a, IntVal b) -> IntVal (b - a)
        (Mul, IntVal a, IntVal b) -> IntVal (a * b)
        (Div, IntVal a, IntVal b) -> if a == 0 then error "Division by zero" else IntVal (b `div` a)
        _ -> error "Invalid operation or operands"
  in exec rest (result : stack)
exec (Ret : _) (v : _) = v
exec _ _ = error "Invalid program or stack state"

-- TEST VM
test_vm :: IO ()
test_vm = do
  let program1 = [Push (IntVal 42), Ret]
  print $ exec program1 []
  let program2 = [Push (IntVal 52), Push (IntVal 10), Call Sub, Ret]
  print $ exec program2 []
-- TEST VM