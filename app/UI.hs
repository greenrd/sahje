{-# LANGUAGE GADTs, NamedFieldPuns #-}
module UI where

import Brick.Main (App(..), continue, defaultMain, halt, showFirstCursor)
import Brick.Types (BrickEvent(VtyEvent), Widget)
import Brick.Widgets.Core ((<+>), hLimit, str, withAttr)
import Brick.Widgets.List (handleListEvent, listElementsL, listSelectedL, renderList)
import ClassyPrelude hiding (maximum, toList)
import Data.Foldable (maximum, toList)
import Data.Maybe (fromJust)
import Data.List.NonEmpty (nonEmpty)
import Graphics.Vty.Input.Events (Event(EvKey), Key(KChar))
import Lens.Micro ((^.), (^?))
import Lens.Micro.Platform (ix)

import Attributes
import Model

render :: Bool -> Model -> Widget Path
render hasFocus (Obj fields)
  = maybe listUI ((listUI <+>) . render False . snd) selectedField
  where
    listUI
      = hLimit . (2 +) . maxWidth . toList <*> renderList fieldUI hasFocus $ fst <$> fields
    maxWidth :: [Text] -> Int
    maxWidth = maybe 0 (maximum . map length) . nonEmpty
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
