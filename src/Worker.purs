module Worker where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Control.Monad.Except (runExcept)
import Foreign (readInt)
import Web.Worker.GlobalScope (postMessage, onMessage)
import Web.Worker.MessageEvent (data_)
import Effect.Console (log)

main :: Effect Unit
main = do
  onMessage \ev -> do
    case runExcept $ readInt $ data_ ev of
      Left _ -> log "Sent data is not an integer"
      Right n -> postMessage $ n * n