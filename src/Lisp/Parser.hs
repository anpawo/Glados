{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Parser
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Lisp.Parser
  ( -- main program
    parseInput,
    runParser,
    parse,
    -- unit-tests
    SExpr (..),
    parseSInt,
    parseSSymbol,
    parseSString,
    parseSList,
    Parser,
    -- ghci tests
    t,
    tt,
    char,
    eof,
    parseTest,
    many,
  )
where

import Control.Applicative ((<|>))
import Data.Text (Text, pack)
import Data.Void (Void)
import Lisp.SExpression (SExpr (..))
import Text.Megaparsec (MonadParsec (eof), Parsec, many, noneOf, oneOf, parse, parseTest, runParser, some, try)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal, float, signed)

type Parser = Parsec Void Text

t :: Parser SExpr -> String -> IO ()
t p s = parseTest p (pack s)

tt :: Parser [SExpr] -> String -> IO ()
tt p s = parseTest p (pack s)

parseSInt :: Parser SExpr
parseSInt = SInt <$> signed mempty decimal

parseSFloat :: Parser SExpr
parseSFloat = SFloat <$> signed mempty float

parseSSymbol :: Parser SExpr
parseSSymbol = SSymbol <$> ((:) <$> noneOf startForbid <*> many (noneOf otherForbid))
  where
    startForbid = "\"()0123456789 \t\n\r\f\v"
    otherForbid = "\"() \t\n\r\f\v"

parseSString :: Parser SExpr
parseSString = SString <$> (char '"' *> many (noneOf "\"") <* char '"') -- can be an empty string

parseSList :: Parser SExpr
parseSList = SList <$> (char '(' *> (((:) <$> try startExpr <*> many otherExpr) <|> pure []) <* many spaces <* char ')')
  where
    startExpr = many spaces *> (try parseSFloat <|> try parseSInt <|> try parseSSymbol <|> parseSString <|> parseSList)
    otherExpr = try (some spaces *> (try parseSFloat <|> try parseSInt <|> try parseSSymbol <|> try parseSString)) <|> try (many spaces *> parseSList)

spaces :: Parser Char
spaces = oneOf " \t\n\r\f\v"

parseInput :: Parser SExpr
parseInput = many spaces *> parseOneExp <* many spaces <* eof
  where
    parseOneExp = try parseSFloat <|> try parseSInt <|> try parseSSymbol <|> try parseSString <|> try parseSList
