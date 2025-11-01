module AI.Perceptron.Core where

import Prelude

type Output = Int

type Input = Int

type Weight = Int

type Weights = { biasWeight :: Weight, weight1 :: Weight, weight2 :: Weight }

type Inputs =
  { bias :: Int
  , input1 :: Input
  , input2 :: Input
  }

heaviside :: Int -> Int
heaviside x = if x >= 0 then 1 else 0

perceptron :: Weights -> Inputs -> Output
perceptron { biasWeight, weight1, weight2 } { bias, input1, input2 } =
  let
    resultBias = biasWeight * bias
    result1 = weight1 * input1
    result2 = weight2 * input2
    sum = resultBias + result1 + result2
  in
    heaviside sum
