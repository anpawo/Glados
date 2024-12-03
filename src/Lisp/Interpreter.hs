{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Virtual Machine
-}

module Lisp.Interpreter (interpreter) where

import Control.Exception (catch)
import Control.Exception.Base (IOException)
import Data.Text (pack)
import Lisp.Ast (Ast, sexprToAST)
import Lisp.Parser (parseInput, runParser)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hFlush, stdout)
import Text.Megaparsec (errorBundlePretty)

-- parseSExpr -> Ast -> Execute -> Return and display result except void
interpreter :: [Ast] -> String -> IO ()
interpreter ctx lastInput = catch runInterpreter eofHandler
  where
    runInterpreter = printPrompt lastInput >> getLine >>= handleInput ctx lastInput

    printPrompt "" = putStr "glados> " >> hFlush stdout
    printPrompt _ = putStr "......> " >> hFlush stdout

    eofHandler :: IOException -> IO ()
    eofHandler _ = return ()

handleInput :: [Ast] -> String -> String -> IO ()
handleInput ctx lastInput "" = interpreter ctx lastInput
handleInput ctx lastInput currInput
  | isWaitingEnd totalInput = interpreter ctx totalInput
  | otherwise = execute ctx totalInput
  where
    totalInput = lastInput ++ currInput -- TODO: add "\n"

isWaitingEnd :: String -> Bool
isWaitingEnd input = sameNumberParent input /= 0 -- TODO: should be higher than 0 else its an error because its a closing parenthesis
  where
    sameNumberParent :: String -> Int
    sameNumberParent "" = 0
    sameNumberParent ('(' : s) = 1 + sameNumberParent s
    sameNumberParent (')' : s) = (-1) + sameNumberParent s
    sameNumberParent ('"' : s) = case dropWhile (/= '"') s of
      "" -> 0
      (_ : rst) -> sameNumberParent rst
    sameNumberParent (_ : s) = 0 + sameNumberParent s

execute :: [Ast] -> String -> IO ()
execute ctx input = case runParser parseInput "" (pack input) of
  Left err -> putStr (errorBundlePretty err) >> exitWith (ExitFailure 84)
  Right expr -> case sexprToAST expr of
    Left err -> putStr err >> exitWith (ExitFailure 84)
    Right ast -> print ast >> interpreter ctx "" -- should execute and add the result to ctx or display it
