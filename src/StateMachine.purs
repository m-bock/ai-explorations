module StateMachine where

import Prelude

import DTS as DTS
import Data.Either (Either)
import Effect (Effect)
import Effect.Class.Console (log)
import GenTS.MyTsBridge (Tok(..))
import Stadium.Core as Stadium
import Stadium.React (useStateMachineSimple)
import TsBridge as TSB

type State =
  { dots :: Array { x :: Int, y :: Int }
  }

init :: State
init =
  { dots: []
  }

data Msg = GotDots (Array { x :: Int, y :: Int })

update :: Msg -> State -> State
update msg state = case msg of
  GotDots dots -> state { dots = dots }

type Dispatchers =
  { gotDots :: Effect Unit
  }

dispatchers :: Stadium.DispatcherApi Msg State Unit -> Dispatchers
dispatchers api =
  { gotDots: log "get dots!"
  }

---

useStateMachine :: Effect { state :: State, dispatch :: Dispatchers }
useStateMachine = useStateMachineSimple
  { update
  , init
  , dispatchers
  }

---

moduleName :: String
moduleName = "StateMachine"

tsExports :: Either TSB.AppError (Array DTS.TsModuleFile)
tsExports = TSB.tsModuleFile moduleName
  [ TSB.tsValues Tok
      { useStateMachine
      }
  ]