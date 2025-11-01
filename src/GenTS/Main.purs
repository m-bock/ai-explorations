module GenTS.Main
  ( main
  ) where

import Prelude

import DTS as DTS
import Data.Either (Either)
import Effect (Effect)
import GenTS.MyTsBridge (class TsBridge, Tok(..))
import TsBridge as TSB
import Unsafe.Coerce (unsafeCoerce)
import StateMachine as StateMachine

myTsProgram :: Either TSB.AppError DTS.TsProgram
myTsProgram =
  TSB.tsProgram
    [ StateMachine.tsExports

    ]

main :: Effect Unit
main = TSB.mkTypeGenCli myTsProgram