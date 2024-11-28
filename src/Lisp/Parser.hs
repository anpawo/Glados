{-
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Parser
-}

module Lisp.Parser () where

import Text.Megaparsec ()
import Text.Megaparsec.Char ()
import Data.Text (Text)
import Data.Void (Void)

type Parser = Parsec Void Text
