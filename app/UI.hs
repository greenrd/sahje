{-# LANGUAGE GADTs, TypeFamilies #-}
module UI where

import Brick.Main (App(..), continue, defaultMain, halt, showFirstCursor)
import Brick.Types (BrickEvent(VtyEvent), Widget)
import Brick.Widgets.Core ((<+>), hLimit, str, textWidth, withAttr)
import Brick.Widgets.List
import ClassyPrelude
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Lazy as HashMap
import Data.JsonSchema.Draft4 (SchemaWithURI(..), Schema(..))
import Data.Validator.Draft4.Object (Dependency(..))
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

checkDep :: List n (Text, Model) -> Text -> (Text, Dependency Schema) -> Bool
checkDep newFields _ (dependentField, _)
  | not $ dependentField `elem` (fst <$> newFields) = True
checkDep newFields deletedField (dependentField, PropertyDependency requiredFields)
  = not $ deletedField `elem` requiredFields
checkDep newFields deletedField (dependentField, SchemaDependency schema)
  = checkDeletion schema newFields deletedField

checkDeletion :: Schema -> List n (Text, Model) -> Text -> Bool
checkDeletion schema newFields deletedField =
  and $ catMaybes [ not . (deletedField `member`) <$> _schemaRequired schema
      , (length newFields >=) <$> _schemaMinProperties schema
      , all (checkDep newFields deletedField) . HashMap.toList <$> _schemaDependencies schema
      ]

deleteSelected :: Schema -> List n (Text, Model) -> Maybe (List n (Text, Model))
deleteSelected schema fields = do
  selectedIndex <- fields ^. listSelectedL
  selField <- fst <$> selectedField fields
  let newFields = listRemove selectedIndex fields
  guard $ checkDeletion schema newFields selField
  pure $ jumpBackIfPossible selectedIndex newFields
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
