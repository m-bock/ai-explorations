module Test.Main where

import Prelude

-- import Data.Time.Duration (Milliseconds(..))
-- import Effect (Effect)
-- import Effect.Aff (launchAff_, delay)
-- import Test.Spec (pending, describe, it)
-- import Test.Spec.Assertions (shouldEqual)
-- import Test.Spec.Reporter.Console (consoleReporter)
-- import Test.Spec.Runner (runSpec)
-- import LibAI as AI

-- main :: Effect Unit
-- main = launchAff_ $ runSpec [ consoleReporter ] do
--   describe "LibAI" do
--     describe "heaviside" do
--       it "should return 1 if the input is greater than 0" do
--         AI.heaviside 1 `shouldEqual` 1
--         AI.heaviside 100 `shouldEqual` 1
--       it "should return 0 if the input is less than or equal to 0" do
--         AI.heaviside 0 `shouldEqual` 0
--         AI.heaviside (-1) `shouldEqual` 0
--         AI.heaviside (-100) `shouldEqual` 0

--     describe "perceptron" do
--       it "should return 1 if the input is greater than 0" do
--         AI.perceptron { weight1: 1, weight2: 1 } { input1: 1, input2: 1 } `shouldEqual` 2
--         AI.perceptron { weight1: 1, weight2: 1 } { input1: 100, input2: 100 } `shouldEqual` 1
--       it "should return 0 if the input is less than or equal to 0" do
--         AI.perceptron { weight1: 1, weight2: 1 } { input1: 0, input2: 0 } `shouldEqual` 0
--         AI.perceptron { weight1: 1, weight2: 1 } { input1: -1, input2: -1 } `shouldEqual` 0
--         AI.perceptron { weight1: 1, weight2: 1 } { input1: -100, input2: -100 } `shouldEqual` 0

