module Model where

import Brick.Widgets.List (List, list)
import ClassyPrelude
import Data.Aeson.Types (Value(Object))
import Data.JsonSchema.Draft4 (Schema, SchemaWithURI)

data Model = Obj (List () Text) | Other

data SahjeState = SahjeState { schema   :: SchemaWithURI Schema
                             , filename :: FilePath
                             , model    :: Model
                             }

toModel (Object fieldMap) = Obj $ list () (fromList . sort $ keys fieldMap) 1
toModel _ = Other
