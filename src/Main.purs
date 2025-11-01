module Main where

import Prelude

import AI.Logging (writeLogToCsv)
import AI.Perceptron.Core (Weights, perceptron)
import AI.Perceptron.Train (train, trainWithLog)
import Conv (class IsInputs, class IsOutput, toInputs, toOutput, fromOutput, fromInputs)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)

---

data EmployerOutcome = IsGood | IsBad

derive instance Eq EmployerOutcome

derive instance Generic EmployerOutcome _

instance Show EmployerOutcome where
  show = genericShow

---

data EmployeeStatus
  = Working
  | Resting

derive instance Eq EmployeeStatus

derive instance Generic EmployeeStatus _

instance Show EmployeeStatus where
  show = genericShow

---

newtype TeamPerDay = TeamPerDay
  { herrLauch :: EmployeeStatus
  , frauKarotte :: EmployeeStatus
  }

derive newtype instance Show TeamPerDay

newtype Sample = Sample
  { teamPerDay :: TeamPerDay
  , employerOutcome :: EmployerOutcome
  }

getResultSimple :: TeamPerDay -> EmployerOutcome
getResultSimple (TeamPerDay { herrLauch, frauKarotte }) = case herrLauch, frauKarotte of
  Working, Working -> IsGood
  Working, Resting -> IsGood
  Resting, Working -> IsGood
  Resting, Resting -> IsBad

---

samples :: Array (TeamPerDay /\ EmployerOutcome)
samples =
  [ (TeamPerDay { herrLauch: Working, frauKarotte: Working } /\ IsGood)
  , (TeamPerDay { herrLauch: Working, frauKarotte: Resting } /\ IsGood)
  , (TeamPerDay { herrLauch: Resting, frauKarotte: Working } /\ IsGood)
  , (TeamPerDay { herrLauch: Resting, frauKarotte: Resting } /\ IsBad)
  ]

testfn
  :: forall inputs output
   . IsInputs inputs
  => IsOutput output
  => Eq output
  => Show inputs
  => Show output
  => (inputs -> output)
  -> (Array (inputs /\ output))
  -> Effect Unit
testfn fn samples = do
  for_ samples \(input /\ output) -> do
    let
      actualOutput = fn input
    if actualOutput == output then do
      pure unit
    else do
      log ("Test failed: " <> show input <> " -> " <> show actualOutput <> " != " <> show output)

--

getResultAI :: Weights -> TeamPerDay -> EmployerOutcome
getResultAI weights team =
  let
    ret = perceptron weights (toInputs team)
  in
    case fromOutput ret of
      Just result -> result
      Nothing -> unsafeCrashWith "Unexpected output"

main :: Effect Unit
main = do
  testfn getResultSimple samples

  let samples' = map (\(team /\ outcome) -> toInputs team /\ toOutput outcome) samples

  let (weights /\ logs) = trainWithLog (samples' <> samples' <> samples' <> samples' <> samples' <> samples' <> samples' <> samples' <> samples' <> samples')

  log ("Weights: " <> show weights)

  writeLogToCsv logs "log.csv"

  testfn (getResultAI weights) samples

  pure unit

---

instance IsOutput EmployerOutcome where
  fromOutput 1 = Just IsGood
  fromOutput 0 = Just IsBad
  fromOutput _ = Nothing
  toOutput IsGood = 1
  toOutput IsBad = 0

instance IsInputs TeamPerDay where
  toInputs (TeamPerDay { herrLauch, frauKarotte }) =
    { bias: 1
    , input1: case herrLauch of
        Working -> 1
        Resting -> 0
    , input2: case frauKarotte of
        Working -> 1
        Resting -> 0
    }

  fromInputs { bias, input1, input2 } = case input1, input2 of
    0, 0 -> Just (TeamPerDay { herrLauch: Resting, frauKarotte: Resting })
    0, 1 -> Just (TeamPerDay { herrLauch: Resting, frauKarotte: Working })
    1, 0 -> Just (TeamPerDay { herrLauch: Working, frauKarotte: Resting })
    1, 1 -> Just (TeamPerDay { herrLauch: Working, frauKarotte: Working })
    _, _ -> Nothing

