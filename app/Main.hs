{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Main
-}

module Main (main) where

import Lisp.Interpreter (interpreter)
import System.Environment (getArgs)

-- could handle some debug flags
handleArgs :: [String] -> IO ()
handleArgs [] = interpreter [] ""
handleArgs flags = print flags

main :: IO ()
main = getArgs >>= handleArgs

-- filename => run the file (in the interpreter, it will then be closed by the oef)
-- no args => interpreter
