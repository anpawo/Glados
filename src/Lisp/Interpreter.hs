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
import Lisp.Ast (Ast (..), Ctx, sexprToAST)
import Lisp.Display (astToString)
import Lisp.Evaluate (evalAst)
import Lisp.Parser (parseInput, runParser)
import System.IO (hFlush, stdout)
import Text.Megaparsec (errorBundlePretty)

-- parseSExpr -> Ast -> Execute -> Return and display result except void
interpreter :: Ctx -> String -> IO ()
interpreter ctx lastInput = catch runInterpreter eofHandler
  where
    runInterpreter = printPrompt lastInput >> getLine >>= handleInput ctx lastInput

    printPrompt "" = putStr "glados> " >> hFlush stdout
    printPrompt _ = putStr "......> " >> hFlush stdout

    eofHandler :: IOException -> IO ()
    eofHandler _ = return ()

handleInput :: Ctx -> String -> String -> IO ()
handleInput ctx lastInput "" = interpreter ctx lastInput
handleInput ctx lastInput (':' : currInput) = handleCommand ctx currInput >> interpreter ctx lastInput
handleInput ctx lastInput currInput
  | isWaitingEnd totalInput = interpreter ctx totalInput
  | otherwise = execute ctx totalInput
  where
    totalInput = lastInput ++ "\n" ++ currInput

handleCommand :: Ctx -> String -> IO ()
handleCommand ctx "ctx" = print ctx
handleCommand _ _ = putStrLn "Invalid Command"

isWaitingEnd :: String -> Bool
isWaitingEnd input = sameNumberParent input > 0
  where
    sameNumberParent :: String -> Int
    sameNumberParent "" = 0
    sameNumberParent ('(' : s) = 1 + sameNumberParent s
    sameNumberParent (')' : s) = (-1) + sameNumberParent s
    sameNumberParent ('"' : s) = case dropWhile (/= '"') s of
      "" -> 0
      (_ : rst) -> sameNumberParent rst
    sameNumberParent (_ : s) = 0 + sameNumberParent s

execute :: Ctx -> String -> IO ()
execute ctx input = case runParser parseInput "" (pack input) of
  Left err -> putStr (errorBundlePretty err) >> interpreter ctx ""
  Right expr -> case sexprToAST expr of
    Left err -> putStrLn err >> interpreter ctx ""
    Right ast -> case evalAst ctx ast of
      Left err -> putStrLn err >> interpreter ctx ""
      Right (TVoid, newCtx) -> interpreter newCtx ""
      Right (astToDisplay, newCtx) -> putStrLn (astToString newCtx astToDisplay) >> interpreter newCtx ""
