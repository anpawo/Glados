--
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- Spec
--

import Test.Hspec (describe, hspec)
import TestTemplateSpec (spec)

-- We will use Hspec for unit tests.
-- Main of the tests.
-- Call each test suite here.
-- Every test suite must be named "<TestSuiteName>Spec.hs" according to what it tests.

main :: IO ()
main = hspec $ do
  describe "Template-Test" TestTemplateSpec.spec
