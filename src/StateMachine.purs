module StateMachine where

import Prelude

import AI.Perceptron.Core (Weights)
import AI.Perceptron.Core as Perceptron
import Data.Vector2 (Vec(..))
import Effect (Effect)
import GenTS.MyTsBridge (Tok(..))
import Stadium.Core as Stadium
import Stadium.React (useStateMachineSimple)
import TsBridge (TsExports, TypeVar, tsExportValues)
import Types (Range, Line)
import Unsafe.Coerce (unsafeCoerce)

type State =
  { dots :: Array { x :: Int, y :: Int }
  , weights :: Weights
  , domain :: Vec (Range Number)
  }

type DerivedState =
  { linePoints :: { start :: Vec Number, end :: Vec Number } }

selAll :: State -> DerivedState
selAll state =
  let
    line = Perceptron.line state.weights
  in
    { linePoints: getLinePoints line state.domain }

selXs :: State -> Array Int
selXs state = map _.x state.dots

selYs :: State -> Array Int
selYs state = map _.y state.dots

getLinePoints :: Line -> Vec (Range Number) -> { start :: Vec Number, end :: Vec Number }
getLinePoints _ _ = { start: pure 0.0, end: pure 1.0 }

vecToRec :: forall @a. Vec a -> { x :: a, y :: a }
vecToRec (Vec x y) = { x, y }

init :: State
init =
  { dots: []
  , weights: { biasWeight: 0, weight1: 0, weight2: 0 }
  , domain: Vec { min: 0.0, max: 1.0 } { min: 0.0, max: 1.0 }
  }

data Msg = GotDots (Array { x :: Int, y :: Int })

update :: Msg -> State -> State
update msg state = case msg of
  GotDots dots -> state { dots = dots }

type Dispatchers =
  { getDots :: Effect Unit
  }

dispatchers :: Stadium.DispatcherApi Msg State Unit -> Dispatchers
dispatchers api =
  { getDots: api.emitMsg $ GotDots
      [ { x: 0, y: 0 }
      , { x: 0, y: 1 }
      , { x: 1, y: 0 }
      , { x: 1, y: 1 }
      ]
  }

---

useStateMachine :: Effect { state :: State, dispatch :: Dispatchers }
useStateMachine = useStateMachineSimple
  { update
  , init
  , dispatchers
  }

---

tsExports :: TsExports
tsExports = tsExportValues Tok
  { useStateMachine
  , selXs
  , selYs
  , selAll
  , vecToRec: vecToRec :: Vec (TypeVar "A") -> _
  }
