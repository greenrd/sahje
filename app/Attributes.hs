module Attributes (sahjeAttrMap) where

import Brick.AttrMap (attrMap)
import Brick.Widgets.List (listSelectedFocusedAttr)
import Brick.Util (on)
import ClassyPrelude hiding (on)
import Graphics.Vty.Attributes (black, defAttr, white)

sahjeAttrMap = attrMap defAttr [(listSelectedFocusedAttr, black `on` white)]
