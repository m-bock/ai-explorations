module GenTS.Main
  ( main
  ) where

import Prelude

import DTS as DTS
import Data.Either (Either)
import Data.Vector2 as Data.Vector2
import Effect (Effect)
import GenTS.MyTsBridge (Tok(..))
import StateMachine as StateMachine
import TsBridge (TypeVar, tsExportValues)
import TsBridge as TSB

myTsProgram :: Either TSB.AppError DTS.TsProgram
myTsProgram =
  TSB.tsProgram
    [ StateMachine.tsExports "StateMachine"
    , tsExportValues Tok
        { vec: Data.Vector2.vec :: TypeVar "A" -> _
        }
        "Data.Vector2"
    ]

main :: Effect Unit
main = TSB.mkTypeGenCli myTsProgram