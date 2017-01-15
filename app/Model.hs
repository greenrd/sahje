module Model where

import Brick.Widgets.List (List, list)
import ClassyPrelude hiding (toList)
import Data.Aeson.Types (Value(Object, String))
import Data.JsonSchema.Draft4 (Schema, SchemaWithURI)
import GHC.Exts (toList)

type Path = [Text]

data Model = Obj (List Path (Text, Model))
           | Str Text
           | Other Value

toModel path (Object fieldMap) =
  Obj $ list path (fromList . map toNestedModel . sortBy (comparing fst) $ toList fieldMap) 1
  where
    toNestedModel (n, v) = (n, toModel (n : path) v)
toModel _ (String text) = Str text
toModel _ v = Other v

data SahjeState = SahjeState { schema   :: SchemaWithURI Schema
                             , filename :: FilePath
                             , model    :: Model
                             }
