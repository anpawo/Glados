{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Parser
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Lisp.Parser
  ( SExpr (..),
    pack,
    unpack,
    Text,
    Void,
    parseSInt,
    parseSSymbol,
    parseSString,
    parseSList,
    Parser,
    runParser,
    -- tests
    t,
    tt,
    char,
    eof,
    parseTest,
    many,
    parseInput,
  )
where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Lisp.SExpression (SExpr (..))
import Text.Megaparsec (MonadParsec (eof), Parsec, many, noneOf, oneOf, parseTest, runParser, some, try) -- TODO: skipSome
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal, signed)

type Parser = Parsec Void Text

t :: Parser SExpr -> String -> IO ()
t p s = parseTest p (pack s)

tt :: Parser [SExpr] -> String -> IO ()
tt p s = parseTest p (pack s)

parseSInt :: Parser SExpr
parseSInt = SInt <$> signed mempty decimal

parseSSymbol :: Parser SExpr
parseSSymbol = SSymbol <$> ((:) <$> noneOf startForbid <*> many (noneOf otherForbid))
  where
    startForbid = "\"()0123456789 \t\n\r\f\v"
    otherForbid = "\"() \t\n\r\f\v"

parseSString :: Parser SExpr
parseSString = SString <$> (char '"' *> many (noneOf "\"") <* char '"') -- can be an empty string

parseSList :: Parser SExpr
parseSList = SList <$> (char '(' *> ((:) <$> startExpr <*> many otherExpr) <* many spaces <* char ')')
  where
    startExpr = many spaces *> (try parseSInt <|> try parseSSymbol <|> parseSString <|> parseSList)
    otherExpr = try (some spaces *> (try parseSInt <|> try parseSSymbol <|> try parseSString)) <|> try (many spaces *> parseSList)

spaces :: Parser Char
spaces = oneOf " \t\n\r\f\v"

parseInput :: Parser [SExpr]
parseInput = removeSpaces *> ((eof $> []) <|> ((:) <$> parseOneExp <*> parseInput))
  where
    removeSpaces = many spaces
    parseOneExp = try parseSInt <|> try parseSSymbol <|> try parseSString <|> try parseSList

-- TODO: add tests

-- parseComments :: Parser Void
