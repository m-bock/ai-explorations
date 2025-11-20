module StateMachine where

import Prelude

import AI.Perceptron.Core (Weights)
import AI.Perceptron.Core as Perceptron
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Vector2 (Vec(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import GenTS.MyTsBridge (Tok(..))
import Partial.Unsafe (unsafeCrashWith)
import Stadium.Core as Stadium
import Stadium.React (useStateMachineSimple)
import Stadium.TL (mkConstructors)
import TsBridge (TsExports, TypeVar, tsBridgeOpaqueType, tsExportValues)
import TsBridge.ClassVia (class TsBridgeVia)
import Types (Range, Line)
import Unsafe.Coerce (unsafeCoerce)

type State =
  { dots :: Array { x :: Int, y :: Int }
  , weights :: NonEmptyArray Weights
  , epoch :: Int
  , domain :: Vec (Range Number)
  }

type DerivedState =
  { linePoints :: { start :: Vec Number, end :: Vec Number } }

selAll :: State -> DerivedState
selAll state =
  let
    currentWeights = getMod state.weights state.epoch
    line = Perceptron.line currentWeights
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
  , weights: pure { biasWeight: 0, weight1: 0, weight2: 0 }
  , domain: Vec { min: 0.0, max: 1.0 } { min: 0.0, max: 1.0 }
  , epoch: 0
  }

data Msg
  = GotDots (Array { x :: Int, y :: Int })
  | SetEpoch Int

derive instance Generic Msg _

mkMsg :: _
mkMsg = mkConstructors @Msg

update :: Msg -> State -> State
update msg state = case msg of
  GotDots dots -> state { dots = dots }
  SetEpoch epoch -> state { epoch = epoch }

type Dispatchers =
  { getDots :: Effect Unit
  , setEpoch :: Int -> Effect Unit
  , msg :: EffectFn1 Msg Unit
  }

dispatchers :: Stadium.DispatcherApi Msg State Unit -> Dispatchers
dispatchers api =
  { getDots: api.emitMsg $ GotDots
      [ { x: 0, y: 0 }
      , { x: 0, y: 1 }
      , { x: 1, y: 0 }
      , { x: 1, y: 1 }
      ]
  , setEpoch: api.emitMsg <<< SetEpoch
  , msg: mkEffectFn1 api.emitMsg
  }

getMod :: forall a. NonEmptyArray a -> Int -> a
getMod array index = case NEA.index array modIndex of
  Just value -> value
  Nothing -> unsafeCrashWith "getMod: Index out of bounds"
  where
  modIndex = index `mod` (NEA.length array)

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
  , mkMsg
  }

instance TsBridgeVia Tok Msg where
  tsBridgeVia _ = tsBridgeOpaqueType
    { moduleName: "StateMachine"
    , typeName: "Msg"
    , typeArgs: []
    }
