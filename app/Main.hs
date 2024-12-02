{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Main
-}

module Main (main) where

import Lisp.VirtualMachine (interpreter, runFile)
import System.Environment (getArgs)

-- TEMPORAIRE TEST
import Lisp.Compute (testCompute)

-- could handle some debug flags
handleArgs :: [String] -> IO ()
handleArgs [] = interpreter []
handleArgs (inputFile : _) = runFile inputFile

-- TEMPORAIRE TEST
-- main :: IO ()
-- main = getArgs >>= handleArgs
main :: IO ()
main = testCompute

-- filename => run the file (in the interpreter, it will then be closed by the oef)
-- no args => interpreter
