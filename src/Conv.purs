module Conv where

import Prelude

import Data.Maybe (Maybe)
import AI.Perceptron.Core (Input, Output, Inputs)

class IsInputs a where
  toInputs :: a -> Inputs
  fromInputs :: Inputs -> Maybe a

class IsOutput a where
  fromOutput :: Output -> Maybe a
  toOutput :: a -> Output
