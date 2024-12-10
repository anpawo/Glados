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
        (Eq, IntVal a, IntVal b) -> Right (BoolVal (a == b))
        (Less, IntVal a, IntVal b) -> Right (BoolVal (a < b))
        _ -> Left "Error: You cant do that"
  in case result of
       Left err -> Left err
       Right res -> exec rest (res : stack)
exec (JumpIfFalse n : rest) (BoolVal False : stack) = 
  let skip = take n rest
  in exec (drop n rest) stack
exec (JumpIfFalse _ : rest) (_ : stack) = exec rest stack
exec (Ret : _) (v : _) = Right v
exec _ _ = Left "Error: Invalid program or stack state"

-- TEST VM
test_vm :: IO ()
test_vm = do
  --TESTS UNITAITES PAR LA SUITE
  -- Exemple 1 : Retourne 42
  let program1 = [Push (IntVal 42), Ret]
  print $ exec program1 []

  -- Exemple 2 : Soustraction de 52 et 10 (devrait donner 42)
  let program2 = [Push (IntVal 10), Push (IntVal 52), Call Sub, Ret]
  print $ exec program2 []

  -- Exemple 3 : Division par zéro (devrait donner une erreur)
  let program3 = [Push (IntVal 0), Push (IntVal 42), Call Div, Ret]
  print $ exec program3 []

  -- Exemple 4 : Tentative d'addition entre un Int et un Bool (devrait donner une erreur)
  let program4 = [Push (IntVal 10), Push (BoolVal True), Call Add, Ret]
  print $ exec program4 []

  -- Exemple 5 : Utilisation de `Eq` pour tester l'égalité
  let program5 = [Push (IntVal 10), Push (IntVal 10), Call Eq, Ret]
  print $ exec program5 []  -- Doit retourner True

  -- Exemple 6 : Utilisation de `Less` pour tester la comparaison
  let program6 = [Push (IntVal 10), Push (IntVal 5), Call Less, Ret]
  print $ exec program6 []  -- Doit retourner True

  -- Exemple 6B : Utilisation de `Less` pour tester la comparaison (SUJET)
  let program6 = [Push (IntVal 2), Push (IntVal 5), Call Less, Ret]
  print $ exec program6 []  -- Doit retourner False

  -- Exemple 7 : Utilisation de `JumpIfFalse` pour sauter une instruction
  let program7 = [Push (BoolVal False), JumpIfFalse 2, Push (IntVal 99), Ret]
  print $ exec program7 []  -- Doit sauter la valeur 99 et retourner l'erreur d'absence de `Ret`

  -- Exemple 8 : `JumpIfFalse` où la condition est vraie, ne saute pas
  let program8 = [Push (BoolVal True), JumpIfFalse 2, Push (IntVal 99), Ret]
  print $ exec program8 []  -- Doit retourner 99
