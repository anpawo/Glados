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
data Op = Add | Sub | Mul | Div | Eq | Less
  deriving (Show, Eq)

-- Insts
data Inst = Push Value
          | Call Op
          | Ret
          | JumpIfFalse Int
          | PushArg Int
  deriving (Show, Eq)

-- Aliases
type Stack = [Value]
type Insts = [Inst]
type Args = [Value]

-- Fonction d'exécution
exec :: Args -> Insts -> Stack -> Either String Value
exec _ [] stack = Left "Error: No ret instruction ?"
exec args (Push v : rest) stack = exec args rest (v : stack)
exec args (Call op : rest) (v1 : v2 : stack) = 
  let result = case (op, v1, v2) of
        (Add, IntVal a, IntVal b) -> Right (IntVal (a + b))
        (Sub, IntVal a, IntVal b) -> Right (IntVal (a - b))
        (Mul, IntVal a, IntVal b) -> Right (IntVal (a * b))
        (Div, IntVal _ , IntVal 0) -> Left "Error: You cant divide by 0"
        (Div, IntVal a, IntVal b) -> Right (IntVal (a `div` b))
        (Eq, IntVal a, IntVal b) -> Right (BoolVal (a == b))
        (Less, IntVal a, IntVal b) -> Right (BoolVal (a < b))
        _ -> Left "Error: You cant do that"
  in case result of
       Left err -> Left err
       Right res -> exec args rest (res : stack)
exec args (JumpIfFalse n : rest) (BoolVal False : stack) = 
  exec args (drop n rest) stack
exec args (JumpIfFalse _ : rest) (_ : stack) = exec args rest stack
exec args (PushArg i : rest) stack =
  if i < 0 || i >= length args
  then Left $ "Error: Invalid argument index " ++ show i
  else exec args rest ((args !! i) : stack)
exec _ (Ret : _) (v : _) = Right v
exec _ _ _ = Left "Error: Invalid program or stack state"

-- Tests func
test_vm :: IO ()
test_vm = do
  -- TESTS UNIT A FAIRE
  -- Test 1 : Simple retour d'une valeur
  let program1 = [Push (IntVal 42), Ret]
  print $ exec [] program1 []  -- Résultat attendu : Right (IntVal 42)

  -- Test 2 : Opérations arithmétiques (52 - 10)
  let program2 = [Push (IntVal 10), Push (IntVal 52), Call Sub, Ret]
  print $ exec [] program2 []  -- Résultat attendu : Right (IntVal 42)

  -- Test 3 : Division par zéro
  let program3 = [Push (IntVal 0), Push (IntVal 42), Call Div, Ret]
  print $ exec [] program3 []  -- Résultat attendu : Left "Error: You cant divide by 0"

  -- Test 4 : Opération invalide (Int + Bool)
  let program4 = [Push (IntVal 10), Push (BoolVal True), Call Add, Ret]
  print $ exec [] program4 []  -- Résultat attendu : Left "Error: You cant do that"

  -- Test 5 : Comparaison d'égalité
  let program5 = [Push (IntVal 10), Push (IntVal 10), Call Eq, Ret]
  print $ exec [] program5 []  -- Résultat attendu : Right (BoolVal True)

  -- Test 6 : Comparaison "moins que"
  let program6 = [Push (IntVal 10), Push (IntVal 5), Call Less, Ret]
  print $ exec [] program6 []  -- Résultat attendu : Right (BoolVal True)

  -- Test 7 : Saut conditionnel (ne saute pas)
  let program7 = [Push (BoolVal True), JumpIfFalse 2, Push (IntVal 99), Ret]
  print $ exec [] program7 []  -- Résultat attendu : Right (IntVal 99)

  -- Test 8 : Saut conditionnel (saute)
  let program8 = [Push (BoolVal False), JumpIfFalse 2, Push (IntVal 99), Ret]
  print $ exec [] program8 []  -- Résultat attendu : Left "Error: No ret instruction ?"

  -- Test 9 : Utilisation de `PushArg`
  let program9 = [PushArg 0, Push (IntVal 2), Call Mul, Ret]
  print $ exec [IntVal 21] program9 []  -- Résultat attendu : Right (IntVal 42)

  -- Test 10 : Accès invalide à un argument
  let program10 = [PushArg 1, Ret]
  print $ exec [IntVal 42] program10 []  -- Résultat attendu : Left "Error: Invalid argument index 1"
