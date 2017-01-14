{-# LANGUAGE GADTs #-}
module UI where

import Brick.Main (App(..), continue, defaultMain, halt, showFirstCursor)
import Brick.Types (BrickEvent(VtyEvent), Widget)
import Brick.Widgets.Core (str, withAttr)
import Brick.Widgets.List
import ClassyPrelude
import Graphics.Vty.Input.Events (Event(EvKey), Key(KChar))

import Attributes
import Model

ui :: SahjeState -> [Widget ()]
ui (SahjeState { model = Obj fields }) = [renderList fieldUI True fields]
  where
    fieldUI False fieldName = str $ unpack fieldName
    fieldUI True  fieldName = withAttr selected $ fieldUI False fieldName
ui ss = [str "TODO"]

handleEvent ss (VtyEvent (EvKey (KChar 'q') [])) = halt ss
handleEvent ss@(SahjeState { model = Obj fields }) (VtyEvent event) = do
  newFields <- handleListEvent event fields
  continue ss { model = Obj newFields }
handleEvent ss _ = continue ss

app :: App SahjeState () ()
app = App { appDraw = ui
          , appChooseCursor = showFirstCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = \_ -> sahjeAttrMap
          }

brickMain :: SahjeState -> IO ()
brickMain = (() <$) . defaultMain app
