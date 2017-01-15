{-# LANGUAGE GADTs, TypeFamilies #-}
module UI where

import Brick.Main (App(..), continue, defaultMain, halt, showFirstCursor)
import Brick.Types (BrickEvent(VtyEvent), Widget)
import Brick.Widgets.Core ((<+>), hLimit, str, textWidth, withAttr)
import Brick.Widgets.List (handleListEvent, List, listElementsL, listSelectedL, renderList)
import ClassyPrelude
import qualified Data.Foldable as Foldable
import Data.NonNull (fromNullable)
import Graphics.Vty.Input.Events (Event(EvKey), Key(KChar))
import Lens.Micro ((^.), (^?))
import Lens.Micro.Platform (ix)

import Attributes
import Model

type instance Element (List n a) = a

instance MonoFoldable (List n a) where
  otoList = Foldable.toList
  onull = Foldable.null

render :: Bool -> Model -> Widget Path
render hasFocus (Obj fields)
  = maybe listUI ((listUI <+>) . render False . snd) selectedField
  where
    listUI = hLimit . (2 +) . maxWidth <*> renderList fieldUI hasFocus $ fst <$> fields
    maxWidth :: List n Text -> Int
    maxWidth = maybe 0 maximum . fromNullable . map textWidth
    fieldUI fieldSelected fieldName = str $ unpack fieldName
    selectedField :: Maybe (Text, Model)
    selectedField = do
      selectedFieldIndex <- fields ^. listSelectedL
      fields ^? listElementsL . ix selectedFieldIndex
render _ (Str text) = str $ unpack text
render _ _ = str "TODO"

ui :: SahjeState -> [Widget Path]
ui = pure . render True . model

handleEvent ss (VtyEvent (EvKey (KChar 'q') [])) = halt ss
handleEvent ss@(SahjeState { model = Obj fields }) (VtyEvent event) = do
  newFields <- handleListEvent event fields
  continue ss { model = Obj newFields }
handleEvent ss _ = continue ss

app :: App SahjeState () Path
app = App { appDraw = ui
          , appChooseCursor = showFirstCursor
          , appHandleEvent = handleEvent
          , appStartEvent = pure
          , appAttrMap = \_ -> sahjeAttrMap
          }

brickMain :: SahjeState -> IO ()
brickMain = (() <$) . defaultMain app
