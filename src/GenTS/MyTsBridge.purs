module GenTS.MyTsBridge where

import Data.Tuple.Nested ((/\))
import Data.Vector2 as Data.Vector2
import Data.Array.NonEmpty as Data.Array.NonEmpty
import TsBridge (class TsBridgeBy, tsBridgeOpaqueType)
import TsBridge.ClassVia (class TsBridgeVia, tsBridgeVia)
import Type.Proxy (Proxy(..))

data Tok = Tok

instance (TsBridgeVia Tok a) => TsBridgeBy Tok a where
  tsBridgeBy _ = tsBridgeVia Tok

instance (TsBridgeVia Tok a) => TsBridgeVia Tok (Data.Vector2.Vec a) where
  tsBridgeVia tok = tsBridgeOpaqueType
    { moduleName: "Data.Vector2"
    , typeName: "Vec"
    , typeArgs:
        [ "A" /\ tsBridgeVia tok (Proxy :: _ a)
        ]
    }

instance (TsBridgeVia Tok a) => TsBridgeVia Tok (Data.Array.NonEmpty.NonEmptyArray a) where
  tsBridgeVia tok = tsBridgeOpaqueType
    { moduleName: "Data.Array.NonEmpty"
    , typeName: "NonEmptyArray"
    , typeArgs:
        [ "A" /\ tsBridgeVia tok (Proxy :: _ a)
        ]
    }
