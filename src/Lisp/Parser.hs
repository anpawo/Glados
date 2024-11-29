{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Parser
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant section" #-}

module Lisp.Parser
  ( Parsec,
    choice,
    many,
    parse,
    parseTest,
    some,
    try,
    SExpr (..),
    Text,
    pack,
    Void,
    char,
    newline,
    string,
    decimal,
    hexadecimal,
    signed,
    symbol,
    t,
    parseSInt,
    letterChar,
    parseSSymbol,
    between,
    anySingle,
    parseSString,
    parseSList,
    parsers,
  )
where

import Control.Applicative ((<|>))
import Data.Text (Text, pack)
import Data.Void (Void)
import Lisp.SExpression (SExpr (..))
import Text.Megaparsec (Parsec, anySingle, between, choice, many, noneOf, parse, parseTest, skipSome, some, try)
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal, hexadecimal, signed, symbol)

-- import Text.Megaparsec.Combinator (some)

type Parser = Parsec Void Text

t :: Parser SExpr -> String -> IO ()
t p s = parseTest p (pack s)

parseSInt :: Parser SExpr
parseSInt = SInt <$> signed mempty decimal

parseSSymbol :: Parser SExpr
parseSSymbol = SSymbol <$> ((:) <$> letterChar "tu dois fixer le fait que tu puisses appeler des fonctions = ou -)" <*> many alphaNumChar) -- at least one letter at first

parseSString :: Parser SExpr
parseSString = SString <$> (char '"' *> many (noneOf "\"") <* char '"') -- can be an empty string

parseSList :: Parser SExpr
-- parseSList = SList <$> (char '(' *> (toList <$> oneExpr) <* many space <* char ')')
parseSList = SList <$> (char '(' *> ((:) <$> oneExpr <*> many ((some (char ' ') *>) oneExpr)) <* char ')')
  where
    oneExpr = parsers

parsers :: Parser SExpr
parsers = parseSSymbol <|> parseSInt <|> parseSString <|> parseSList

-- parseOneExpression :: Parser SExpr
--     parseSpace
--     (parseInt -> parseInput)
--     (parseSymbol -> parseInput)
--     (parseString -> parseInput)
--     (parseList -> parseInput)
--     parseEof

-- parseInput :: Parser SExpr
-- parseInput = parseOneExpression

-- parseComments :: Parser Void
