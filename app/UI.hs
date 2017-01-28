{-# LANGUAGE GADTs, TypeFamilies #-}
module UI where

import Brick.Main (App(..), continue, defaultMain, halt, showFirstCursor)
import Brick.Types (BrickEvent(VtyEvent), Widget)
import Brick.Widgets.Core ((<+>), hLimit, str, textWidth, withAttr)
import Brick.Widgets.List
import ClassyPrelude
import qualified Data.Foldable as Foldable
import Data.JsonSchema.Draft4 (SchemaWithURI(..), Schema(..))
import Data.NonNull (fromNullable)
import Graphics.Vty.Input.Events (Event(EvKey), Key(KChar, KDel))
import Lens.Micro ((^.), (^?), (.~), to)
import Lens.Micro.Platform (ix)

import Attributes
import Model

type instance Element (List n a) = a

instance MonoFoldable (List n a) where
  otoList = Foldable.toList
  onull = Foldable.null

selectedField :: List n (Text, a) -> Maybe (Text, a)
selectedField fields = do
  selectedFieldIndex <- fields ^. listSelectedL
  fields ^? listElementsL . ix selectedFieldIndex

render :: Bool -> Model -> Widget Path
render hasFocus (Obj fields)
  = maybe listUI ((listUI <+>) . render False . snd) $ selectedField fields
  where
    listUI = hLimit . (2 +) . maxWidth <*> renderList fieldUI hasFocus $ fst <$> fields
    maxWidth :: List n Text -> Int
    maxWidth = maybe 0 maximum . fromNullable . map textWidth
    fieldUI fieldSelected fieldName = str $ unpack fieldName
render _ (Str text) = str $ unpack text
render _ _ = str "TODO"

ui :: SahjeState -> [Widget Path]
ui = pure . render True . model

deleteSelected :: Schema -> List n (Text, Model) -> Maybe (List n (Text, Model))
deleteSelected schema fields = do
  selectedIndex <- fields ^. listSelectedL
  selField <- selectedField fields
  guard $ maybe True (not . ((fst selField) `member`)) $ _schemaRequired schema
  pure . (jumpBackIfPossible <*> (`listRemove` fields)) $ selectedIndex 
  where
    jumpBackIfPossible selectedIndex fields =
      (if selectedIndex < length fields
      then
        listSelectedL .~ pure selectedIndex
      else
        id) fields

handleEvent ss (VtyEvent (EvKey (KChar 'q') [])) = halt ss
handleEvent ss@(SahjeState { model = Obj fields }) (VtyEvent (EvKey KDel [])) =
  let newFields = fromMaybe fields $ deleteSelected (_swSchema $ schema ss) fields
  in continue ss { model = Obj newFields } -- TODO: Use a lens for this
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
