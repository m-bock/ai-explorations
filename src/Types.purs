module Types where

import Prelude

type Line =
  { slope :: Number
  , yIntercept :: Number
  }

type Range a =
  { min :: a
  , max :: a
  }
