{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Parser
-}

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
  )
where

import Data.Text (Text, pack)
import Data.Void (Void)
import Lisp.SExpression (SExpr (..))
import Text.Megaparsec (Parsec, anySingle, between, choice, many, noneOf, parse, parseTest, some, try)
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, newline, string)
import Text.Megaparsec.Char.Lexer (decimal, hexadecimal, signed, symbol)

-- import Text.Megaparsec.Combinator (some)

type Parser = Parsec Void Text

t :: Parser SExpr -> String -> IO ()
t p s = parseTest p (pack s)

parseSInt :: Parser SExpr
parseSInt = SInt <$> signed mempty decimal

parseSSymbol :: Parser SExpr
parseSSymbol = SSymbol <$> ((:) <$> letterChar <*> many alphaNumChar)

parseSString :: Parser SExpr
parseSString = SString <$> (char '"' *> many (noneOf "\"") <* char '"')

parseSList :: Parser SExpr
parseSList = SList <$> p

parseInsideList :: Parser SExpr
parseInsideList = choice
    [ 
    ]

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
