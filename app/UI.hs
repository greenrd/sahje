module UI where

import Brick
import ClassyPrelude
import Data.Aeson.Types (Value)
import Data.JsonSchema.Draft4 (Schema, SchemaWithURI)

ui :: Widget ()
ui = str "File parsed successfully!"

brickMain :: SchemaWithURI Schema -> FilePath -> Value -> IO ()
brickMain schema filename json = simpleMain ui
