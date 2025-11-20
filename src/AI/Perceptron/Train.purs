module AI.Perceptron.Train where

import Prelude

import AI.Logging (class IsTabularLogging, TabularLogValue(..))
import AI.Perceptron.Core (Inputs, Output, Weights, perceptron)
import Control.Monad.Writer (Writer, runWriter)
import Control.Monad.Writer.Class (tell)
import Data.Foldable (foldM)
import Data.Tuple.Nested (type (/\), (/\))

type Deviation = Int

type EpochLogEntry =
  { weights :: Weights
  , inputs :: Inputs
  , actualOutput :: Output
  , expectedOutput :: Output
  , deviation :: Deviation
  }

newtype EpochLogs = EpochLogs (Array EpochLogEntry)

derive newtype instance Semigroup EpochLogs
derive newtype instance Monoid EpochLogs

instance IsTabularLogging EpochLogs where
  toTabularLogging (EpochLogs entries) =
    { headers:
        [ "BiasWeight"
        , "Weight1"
        , "Weight2"
        , "Bias"
        , "Input1"
        , "Input2"
        , "ExpectedOutput"
        , "ActualOutput"
        , "Deviation"
        ]
    , rows: map
        ( \{ inputs, expectedOutput, actualOutput, deviation, weights } ->
            [ ValInt weights.biasWeight
            , ValInt weights.weight1
            , ValInt weights.weight2
            , ValInt inputs.bias
            , ValInt inputs.input1
            , ValInt inputs.input2
            , ValInt expectedOutput
            , ValInt actualOutput
            , ValInt deviation
            ]
        )
        entries
    }

trainEpoch :: Weights -> (Inputs /\ Output) -> Writer EpochLogs Weights
trainEpoch oldWeights (inputs /\ expectedOutput) = do
  let actualOutput = perceptron oldWeights inputs
  let deviation = expectedOutput - actualOutput

  tell (EpochLogs [ { inputs, expectedOutput, actualOutput, deviation, weights: oldWeights } ])

  let newWeights = improveWeights inputs deviation oldWeights
  pure newWeights

improveWeights :: Inputs -> Deviation -> Weights -> Weights
improveWeights { bias, input1, input2 } deviation weights =
  { biasWeight: weights.biasWeight + deviation * bias
  , weight1: weights.weight1 + deviation * input1
  , weight2: weights.weight2 + deviation * input2
  }

trainEpochs :: Array (Inputs /\ Output) -> Writer EpochLogs Weights
trainEpochs trainingSamples = do
  foldM trainEpoch { biasWeight: 0, weight1: 0, weight2: 0 } trainingSamples

train :: Array (Inputs /\ Output) -> Weights
train trainingSamples =
  let
    (weights /\ _) = trainWithLog trainingSamples
  in
    weights

trainWithLog :: Array (Inputs /\ Output) -> (Weights /\ EpochLogs)
trainWithLog trainingSamples =
  runWriter (trainEpochs trainingSamples)