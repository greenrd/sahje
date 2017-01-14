module Attributes where

import Brick.AttrMap (attrMap, AttrName)
import ClassyPrelude
import Graphics.Vty.Attributes (black, defAttr, white, withBackColor, withForeColor)

selected :: AttrName
selected = "selected"

inverted = defAttr `withForeColor` black `withBackColor` white

sahjeAttrMap = attrMap defAttr [(selected, inverted)]
