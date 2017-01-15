module Attributes where

import Brick.AttrMap (attrMap, AttrName)
import Brick.Widgets.List (listSelectedFocusedAttr)
import ClassyPrelude
import Graphics.Vty.Attributes (black, defAttr, white, withBackColor, withForeColor)

inverted = defAttr `withForeColor` black `withBackColor` white

sahjeAttrMap = attrMap defAttr [(listSelectedFocusedAttr, inverted)]
