--
-- EPITECH PROJECT, 2024
-- glados
-- File description:
-- TestTemplateSpec
--

-- Export the test suite name
module TestTemplateSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)

-- To create a test suite you need to:
--    1. name the scope of the tests (after "describe")
--    2. name each following test (after "it")
--    3. add your logic at the left of "`shouldBe`" and the result at the right

spec :: Spec
spec = do
  -- template-test-1
  describe "scope: Boolean Tests" $ do
    it "test-name: true equals true" $
      True == True `shouldBe` True
    it "test-name: false unequals true" $
      True == False `shouldBe` False

  -- template-test-2
  describe "scope: Addition Tests" $ do
    it "1 + 1 equals 2" $
      1 + 1 `shouldBe` 2
    it "1 + 2 equals 3" $
      1 + 2 `shouldBe` 3
