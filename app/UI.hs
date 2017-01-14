{-# LANGUAGE GADTs #-}
module UI where

import Brick.AttrMap (attrMap)
import Brick.Main (App(..), defaultMain, halt)
import Brick.Types (Widget)
import Brick.Widgets.Core (str)
import Brick.Widgets.List
import ClassyPrelude
import Data.Aeson.Types (Value(Object))
import Data.JsonSchema.Draft4 (Schema, SchemaWithURI)
import Graphics.Vty.Attributes (defAttr)

data SahjeState = SahjeState { schema   :: SchemaWithURI Schema
                             , filename :: FilePath
                             , json     :: Value
                             }

toModel :: (Eq a, Ord a, Hashable a) => HashMap a Value -> List () a
toModel obj = list () (fromList $ sort fieldNames) 1
  where
    fieldNames = keys obj

ui :: SahjeState -> [Widget ()]
ui (SahjeState { json = Object obj }) = [renderList fieldUI True $ toModel obj]
  where
    fieldUI selected fieldName = str $ unpack fieldName
ui ss = [str "TODO"]

app :: App SahjeState () ()
app = App { appDraw = ui
          , appChooseCursor = \_ _ -> Nothing
          , appHandleEvent = \s _ -> halt s
          , appStartEvent = return
          , appAttrMap = \_ -> attrMap defAttr []
          }

brickMain :: SahjeState -> IO ()
brickMain = (() <$) . defaultMain app
