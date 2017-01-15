{-# LANGUAGE GADTs, NamedFieldPuns #-}
module UI where

import Brick.Main (App(..), continue, defaultMain, halt, showFirstCursor)
import Brick.Types (BrickEvent(VtyEvent), Widget)
import Brick.Widgets.Core ((<+>), str, withAttr)
import Brick.Widgets.List (handleListEvent, listElementsL, listSelectedL, renderList)
import ClassyPrelude
import Data.Maybe (fromJust)
import Graphics.Vty.Input.Events (Event(EvKey), Key(KChar))
import Lens.Micro ((^.), (^?))
import Lens.Micro.Platform (ix)

import Attributes
import Model

render :: Bool -> Model -> Widget Path
render hasFocus (Obj fields)
  = maybe listUI ((listUI <+>) . render False . snd) selectedField
  where
    listUI = renderList fieldUI hasFocus $ fst <$> fields
    fieldUI fieldSelected fieldName = str $ unpack fieldName
    selectedField :: Maybe (Text, Model)
    selectedField = do
      selectedFieldIndex <- fields ^. listSelectedL
      fields ^? listElementsL . ix selectedFieldIndex
render _ (Str text) = str $ unpack text
render _ _ = str "TODO"

ui :: SahjeState -> [Widget Path]
ui (SahjeState { model }) = [render True model]

handleEvent ss (VtyEvent (EvKey (KChar 'q') [])) = halt ss
handleEvent ss@(SahjeState { model = Obj fields }) (VtyEvent event) = do
  newFields <- handleListEvent event fields
  continue ss { model = Obj newFields }
handleEvent ss _ = continue ss

app :: App SahjeState () Path
app = App { appDraw = ui
          , appChooseCursor = showFirstCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = \_ -> sahjeAttrMap
          }

brickMain :: SahjeState -> IO ()
brickMain = (() <$) . defaultMain app
