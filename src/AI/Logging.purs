module AI.Logging where

import Prelude

import Data.String as Str
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS

class IsTabularLogging a where
  toTabularLogging :: a -> TabularLogging

type TabularLogging =
  { headers :: Array String
  , rows :: Array (Array TabularLogValue)
  }

data TabularLogValue
  = ValStr String
  | ValInt Int

toCsv :: TabularLogging -> String
toCsv { headers, rows } =
  let
    headersString = Str.joinWith "," headers
    rowsString = Str.joinWith "\n" (map toCsvRow rows)
  in
    headersString <> "\n" <> rowsString

toCsvRow :: Array TabularLogValue -> String
toCsvRow row =
  Str.joinWith "," (map toCsvValue row)

toCsvValue :: TabularLogValue -> String
toCsvValue (ValStr str) = str
toCsvValue (ValInt int) = show int

writeLogToCsv :: forall a. IsTabularLogging a => a -> String -> Effect Unit
writeLogToCsv logs filePath = do
  let csv = toCsv (toTabularLogging logs)
  FS.writeTextFile UTF8 filePath csv