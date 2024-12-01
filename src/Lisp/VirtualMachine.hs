{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Virtual Machine
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use print" #-}

module Lisp.VirtualMachine (runFile, interpreter) where

import Control.Exception (catch)
import Control.Exception.Base (IOException)
import Data.Text (pack)
import Lisp.Ast (Ast)
import Lisp.Parser (parseInput, runParser)
import System.IO (hFlush, stdout)
import Text.Megaparsec (errorBundlePretty)

runFile :: String -> IO ()
runFile = putStrLn

-- parseSExpr -> Ast -> Execute -> Return and display result except void
interpreter :: [Ast] -> IO ()
interpreter ctx = catch runInterpreter eofHandler
  where
    runInterpreter = printPrompt >> getLine >>= handleInput >> interpreter ctx

    printPrompt = putStr "> " >> hFlush stdout

    handleInput "" = return ()
    handleInput input = case runParser parseInput "" (pack input) of
      Left err -> putStr (errorBundlePretty err)
      Right expr -> putStrLn $ show expr

    eofHandler :: IOException -> IO ()
    eofHandler _ = return ()
